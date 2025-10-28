!> \file    mo_river_router.f90
!> \copydoc mo_river_router

!> \brief   River router.
!> \details This module contains a router for river networks.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    May 2025
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! This code is released under the LGPLv3+ license \license_note
module mo_river_router

  use mo_kind, only: i4, i8, dp
  use mo_constants, only: nodata_i4, nodata_dp
  use mo_utils, only: equal, optval
  use mo_string_utils, only: n2s => num2str
  use mo_river, only: river_t
  use mo_grid, only: grid_t, bottom_up, cartesian
  use mo_grid_scaler, only: scaler_t, up_sum, down_nearest, down_scaling
  use mo_message, only: error_message
  use mo_datetime, only: HOUR_SECONDS
  use mo_netcdf, only: NcDataset, NcVariable, NcDimension

  implicit none
  private

  !> \name Routing Constants
  !> \brief Constants controlling the routing algorithms.
  !!@{
  !> valid routing time steps [s] (integer minute fractions of hour or integer hour fractions of day)
  real(dp), public, dimension(19), parameter :: routing_steps = &
    [   60.0_dp,   120.0_dp,   180.0_dp,   240.0_dp,   300.0_dp,   360.0_dp,            &
       600.0_dp,   720.0_dp,   900.0_dp,  1200.0_dp,  1800.0_dp,  3600.0_dp,            &
      7200.0_dp, 10800.0_dp, 14400.0_dp, 21600.0_dp, 28800.0_dp, 43200.0_dp, 86400.0_dp ]
  !> space weighting of routing is set to 0, since this parameter has no effect on the routing results, see Thober et al. 2017
  real(dp), public, parameter :: routing_space_weight = 0.0_dp
  ! time slice selector in muskingum schema for link_inflow and link_routed
  integer(i4), public, parameter :: current = 1_i4 !< selector for current time step
  integer(i4), public, parameter :: previous = 2_i4 !< selector for previous time step
  real(dp), public, parameter :: liter_to_m3 = 0.001_dp !< conversion factor from liter to m3
  !!@}

  !> \class inflow_t
  !> \brief Inflow handler
  type, public :: inflow_t
    integer(i8) :: count = 0_i8 !< number of inflow nodes
    integer(i8), allocatable :: nodes(:) !< list of inflow nodes [id]
    logical, allocatable :: headwater(:) !< whether to consider headwater cells of inflow gauge
    real(dp), allocatable :: rate(:) !< inflow rate [m3 s-1]
  ! contains
    ! procedure, public :: init => inflow_init
    ! procedure, public :: add => inflow_add
  end type inflow_t

  !> \class river_router_t
  !> \brief River network router
  type, public :: river_router_t
    type(river_t), pointer :: river => null() !< river definition to route on
    type(grid_t), pointer :: input_grid => null() !< grid the input (e.g. runoff) is defined on
    type(scaler_t) :: scaler !< input rescaler
    integer(i4) :: input_step !< [h] time step size of the input in hours
    integer(i4) :: input_count !< [-] number of accumulated inputs for longer output step
    integer(i4) :: output_step !< [h] time step size of the output in hours (at least 1)
    real(dp) :: step !< [s] time step size of the routing (for meeting the CFL condition)
    integer(i4) :: iterations !< number of routing iterations for each output step
    integer(i4) :: accumulations !< number of input accumulations for each output step
    type(inflow_t) :: inflow_handler !< inflow handler
    !> [mm] accumulated runoff size(input_grid\%ncells)
    real(dp), allocatable :: acc_runoff(:)
    !> [m3 s-1] runoff flux for current cell as intermediate result for SCC, size(river\%grid\%ncells)
    real(dp), allocatable :: scaled_runoff(:)
    !> [m3 s-1] runoff flux to route for current river node, size(river\%n_nodes)
    real(dp), allocatable :: runoff(:)
    !> [m3 s-1] inflow flux to link starting at current node from current time step, size(river\%n_nodes)
    real(dp), allocatable :: discharge(:)
    !> [m3 s-1] inflow flux to link starting at current node from previous time step, size(river\%n_nodes)
    real(dp), allocatable :: previous_discharge(:)
    !> [m3 s-1] routed flux from link starting at current node from current time step, size(river\%n_nodes)
    real(dp), allocatable :: tributary(:)
    !> [m3 s-1] routed flux from link starting at current node from previous time step, size(river\%n_nodes)
    real(dp), allocatable :: previous_tributary(:)
    real(dp), allocatable :: nu1(:) !< Muskingum parameter nu1, size(river\%n_nodes) -> C1 = nu2, C2 = nu1-nu2, C3=1-nu1
    real(dp), allocatable :: nu2(:) !< Muskingum parameter nu2, size(river\%n_nodes) -> C1 = nu2, C2 = nu1-nu2, C3=1-nu1
    !> minimum size of river-levels to route in parallel (default: threads * 8, 0 to run all in serial, 1 to run all in parallel)
    integer(i8) :: omp_level_thresh = 0_i8
    integer(i8) :: last_parallel_level = 0_i8 !< last level to run in parallel
  contains
    procedure, public :: init => river_router_init
    procedure, public :: init_from_restart => river_router_init_from_restart
    procedure, public :: write_restart_to_dataset => river_router_write_restart_to_dataset
    procedure, public :: update => river_router_update
    procedure, private :: setup_muskingum => river_router_setup_muskingum
    procedure, private :: scale_runoff => river_router_scale_runoff
    procedure, private :: route => river_router_route
    procedure, private :: time_shift => river_router_time_shift
  end type river_router_t

contains

  !> \brief Setup river upscaler from fine river and coarse target grid.
  subroutine river_router_init(this, river, input_grid, input_step, inflow_handler, max_route_step, root_levels, omp_level_thresh)
    !$ use omp_lib, only: omp_get_num_threads
    implicit none
    class(river_router_t), intent(inout) :: this
    type(river_t), pointer, intent(in) :: river !< river definition
    type(grid_t), pointer, intent(in) :: input_grid !< input grid
    integer(i4), intent(in), optional :: input_step !< [h] input time step size (1 by default)
    type(inflow_t), intent(in), optional :: inflow_handler !< inflow specifications
    real(dp), optional, intent(in) :: max_route_step !< [s] maximum routing time step (default: 86400.0)
    logical, intent(in), optional :: root_levels !< order levels as distance from graph roots (default: .false.)
    !> minimum size of river-levels to route in parallel (default: threads * 8, 0 to run all in serial, 1 to run all in parallel)
    integer(i8), optional, intent(in) :: omp_level_thresh
    integer(i8) :: i
    integer(i8), pointer :: level_size(:)
    this%river => river
    if (.not.allocated(this%river%order%id)) call this%river%calc_order(root_levels)
    this%input_grid => input_grid
    call this%scaler%init( &
      source_grid=this%input_grid, &
      target_grid=this%river%grid, &
      upscaling_operator=up_sum, &       ! if L11 coarser than L1: sum runoff
      downscaling_operator=down_nearest) ! if L11 finer than L1: distribute same value on fine cells
    if (present(inflow_handler)) this%inflow_handler = inflow_handler
    this%input_count = 0_i4
    this%input_step = optval(input_step, 1_i4)
    ! only need scaled runoff as intermediate result in case of SCC
    if (this%river%scc) allocate(this%scaled_runoff(this%river%grid%ncells), source=0.0_dp)
    allocate(this%runoff(this%river%n_nodes), source=0.0_dp)
    allocate(this%acc_runoff(this%input_grid%ncells), source=0.0_dp)
    allocate(this%discharge(this%river%n_nodes), source=0.0_dp)
    allocate(this%previous_discharge(this%river%n_nodes), source=0.0_dp)
    allocate(this%tributary(this%river%n_nodes), source=0.0_dp)
    allocate(this%previous_tributary(this%river%n_nodes), source=0.0_dp)
    ! setup muskingum parameters
    call this%setup_muskingum(max_route_step)

    !$omp parallel
    !$ this%omp_level_thresh = int(omp_get_num_threads() * 8, kind=i8)
    !$ if (present(omp_level_thresh)) this%omp_level_thresh = omp_level_thresh
    !$omp end parallel

    ! determine last level to run in parallel
    if (this%river%order%n_levels == 1_i8) then
      if (this%omp_level_thresh > 0_i8) this%last_parallel_level = 1_i8
      return
    end if
    level_size => this%river%order%level_size
    if (all(level_size(:this%river%order%n_levels-1_i8) >= level_size(2_i8:))) then
      if (this%omp_level_thresh > 0_i8) then
        do i = 1_i8, this%river%order%n_levels
          this%last_parallel_level = i
          if (this%river%order%level_size(i) < this%omp_level_thresh) exit
        end do
      end if
    else
      ! root based levels are not sorted in size, so run all in parallel if wanted
      if (this%omp_level_thresh > 0_i8) this%last_parallel_level = this%river%order%n_levels
    end if
  end subroutine river_router_init

  ! !> \brief Setup river upscaler from restart file
  subroutine river_router_init_from_restart(this, restart_nc, river, input_grid, input_step, inflow_handler, max_route_step, root_levels, omp_level_thresh)
    use mo_utils, only: locate
    !$ use omp_lib, only: omp_get_num_threads
    implicit none
    class(river_router_t), intent(inout) :: this
    type(NcDataset), intent(in) :: restart_nc !< restart dataset
    type(river_t), pointer, intent(in) :: river !< river definition
    type(grid_t), pointer, intent(in) :: input_grid !< input grid
    integer(i4), intent(in), optional :: input_step !< [h] input time step size (1 by default)
    type(inflow_t), intent(in), optional :: inflow_handler !< inflow specifications
    type(NcVariable) :: nc_var

    real(dp), allocatable :: k(:), dummy2d(:,:)
    integer(i8) :: i
    integer(i8), pointer :: level_size(:)
    integer(i4) :: step_id
    real(dp), optional, intent(in) :: max_route_step !< [s] maximum routing time step (default: 86400.0)
    logical, intent(in), optional :: root_levels !< order levels as distance from graph roots (default: .false.)
    !> minimum size of river-levels to route in parallel (default: threads * 8, 0 to run all in serial, 1 to run all in parallel)
    integer(i8), optional, intent(in) :: omp_level_thresh

    this%river => river
    this%input_grid => input_grid
    call this%scaler%init( &
      source_grid=this%input_grid, &
      target_grid=this%river%grid, &
      upscaling_operator=up_sum, &       ! if L11 coarser than L1: sum runoff
      downscaling_operator=down_nearest) ! if L11 finer than L1: distribute same value on fine cells
    if (present(inflow_handler)) this%inflow_handler = inflow_handler
    this%input_count = 0_i4
    this%input_step = optval(input_step, 1_i4)
    ! only need scaled runoff as intermediate result in case of SCC
    if (this%river%scc) allocate(this%scaled_runoff(this%river%grid%ncells), source=0.0_dp)
    allocate(this%runoff(this%river%n_nodes), source=0.0_dp)
    allocate(this%acc_runoff(this%input_grid%ncells), source=0.0_dp)
    if (restart_nc%hasVariable("discharge")) then
      nc_var = restart_nc%getVariable("discharge")
      call nc_var%getData(dummy2d)
      allocate(this%discharge(this%river%n_nodes), source=this%river%grid%pack(dummy2d))
      deallocate(dummy2d)
    else
      allocate(this%discharge(this%river%n_nodes), source=0.0_dp)
    end if
    if (restart_nc%hasVariable("previous_discharge")) then
      nc_var = restart_nc%getVariable("previous_discharge")
      call nc_var%getData(dummy2d)
      allocate(this%previous_discharge(this%river%n_nodes), source=this%river%grid%pack(dummy2d))
      deallocate(dummy2d)
    else
      allocate(this%previous_discharge(this%river%n_nodes), source=0.0_dp)
    end if
    if (restart_nc%hasVariable("tributary")) then
      nc_var = restart_nc%getVariable("tributary")
      call nc_var%getData(dummy2d)
      allocate(this%tributary(this%river%n_nodes), source=this%river%grid%pack(dummy2d))
      deallocate(dummy2d)
    else
      allocate(this%tributary(this%river%n_nodes), source=0.0_dp)
    end if
    if (restart_nc%hasVariable("previous_tributary")) then
      nc_var = restart_nc%getVariable("previous_tributary")
      call nc_var%getData(dummy2d)
      allocate(this%previous_tributary(this%river%n_nodes), source=this%river%grid%pack(dummy2d))
      deallocate(dummy2d)
    else
      allocate(this%previous_tributary(this%river%n_nodes), source=0.0_dp)
    end if
    if (restart_nc%hasVariable("nu1")) then
      nc_var = restart_nc%getVariable("nu1")
      call nc_var%getData(dummy2d)
      allocate(this%nu1(this%river%n_nodes), source=this%river%grid%pack(dummy2d))
      deallocate(dummy2d)
    else
      allocate(this%nu1(this%river%n_nodes), source=0.0_dp)
    end if
    if (restart_nc%hasVariable("nu2")) then
      nc_var = restart_nc%getVariable("nu2")
      call nc_var%getData(dummy2d)
      allocate(this%nu2(this%river%n_nodes), source=this%river%grid%pack(dummy2d))
      deallocate(dummy2d)
    else
      allocate(this%nu2(this%river%n_nodes), source=0.0_dp)
    end if

    ! wave travel time parameter [s]
    allocate(k(this%river%n_nodes), source=(this%river%link_length / this%river%celerity))
    step_id = max(1_i4, locate(routing_steps, minval(k, mask=.not.this%river%is_sink)))
    this%step = routing_steps(step_id)
    this%output_step = max(this%input_step, merge(1_i4, nint(this%step, i4) / HOUR_SECONDS, this%step < 3600.0_dp))
    this%iterations = (this%output_step * HOUR_SECONDS) / nint(this%step, i4) ! 1 if step > 3600

    if (this%output_step > this%input_step) then
      this%accumulations = this%output_step / this%input_step
    else
      this%accumulations = 1_i4
    end if

    !$omp parallel
    !$ this%omp_level_thresh = int(omp_get_num_threads() * 8, kind=i8)
    !$ if (present(omp_level_thresh)) this%omp_level_thresh = omp_level_thresh
    !$omp end parallel

    ! determine last level to run in parallel
    if (this%river%order%n_levels == 1_i8) then
      if (this%omp_level_thresh > 0_i8) this%last_parallel_level = 1_i8
      return
    end if
    level_size => this%river%order%level_size
    if (all(level_size(:this%river%order%n_levels-1_i8) >= level_size(2_i8:))) then
      if (this%omp_level_thresh > 0_i8) then
        do i = 1_i8, this%river%order%n_levels
          this%last_parallel_level = i
          if (this%river%order%level_size(i) < this%omp_level_thresh) exit
        end do
      end if
    else
      ! root based levels are not sorted in size, so run all in parallel if wanted
      if (this%omp_level_thresh > 0_i8) this%last_parallel_level = this%river%order%n_levels
    end if
  end subroutine river_router_init_from_restart

  subroutine river_router_write_restart_to_dataset(this, restart_nc, deflate_level)
    use mo_netcdf, only: NcDataset, NcVariable
    implicit none
    class(river_router_t), intent(in) :: this
    integer(i4), intent(in), optional :: deflate_level
    type(NcDataset), intent(inout) :: restart_nc !< restart dataset
    type(NcVariable) :: nc_var
    type(NcDimension) :: node_dim, xdim, ydim
    integer(i4) :: deflate

    deflate = optval(deflate_level, 6_i4)

    if ( this%river%grid%coordsys == cartesian ) then
      xdim = restart_nc%getDimension("x")
      ydim = restart_nc%getDimension("y")
    else 
      xdim = restart_nc%getDimension("lon")
      ydim = restart_nc%getDimension("lat")
    end if

    if ( .not.restart_nc%hasDimension("node") ) then
      node_dim = restart_nc%setDimension("node", int(this%river%n_nodes, i4))  ! only works if network is not to huge for i4
    else
      node_dim = restart_nc%getDimension("node")
    end if

    ! write discharge
    if ( allocated(this%discharge) ) then
      print*, "writing discharge to restart_file"
      nc_var = restart_nc%setVariable("discharge", "f64", [xdim, ydim], deflate_level=deflate, shuffle=.true.)
      call nc_var%setAttribute("long_name", "discharge")
      call nc_var%setFillValue(nodata_dp)
      call nc_var%setAttribute("missing_value", nodata_dp)
      call nc_var%setData(this%river%grid%unpack(this%discharge))
    end if

    ! write previous_discharge
    if ( allocated(this%previous_discharge) ) then
      print*, "writing previous_discharge to restart_file"
      nc_var = restart_nc%setVariable("previous_discharge", "f64", [xdim, ydim], deflate_level=deflate, shuffle=.true.)
      call nc_var%setAttribute("long_name", "previous_discharge")
      call nc_var%setFillValue(nodata_dp)
      call nc_var%setAttribute("missing_value", nodata_dp)
      call nc_var%setData(this%river%grid%unpack(this%previous_discharge))
    end if

    ! write tributary
    if ( allocated(this%tributary) ) then
      print*, "writing tributary to restart_file"
      nc_var = restart_nc%setVariable("tributary", "f64", [xdim, ydim], deflate_level=deflate, shuffle=.true.)
      call nc_var%setAttribute("long_name", "tributary")
      call nc_var%setFillValue(nodata_dp)
      call nc_var%setAttribute("missing_value", nodata_dp)
      call nc_var%setData(this%river%grid%unpack(this%tributary))
    end if

    ! write previous_tributary
    if ( allocated(this%previous_tributary) ) then
      print*, "writing previous_tributary to restart_file"
      nc_var = restart_nc%setVariable("previous_tributary", "f64", [xdim, ydim], deflate_level=deflate, shuffle=.true.)
      call nc_var%setAttribute("long_name", "previous_tributary")
      call nc_var%setFillValue(nodata_dp)
      call nc_var%setAttribute("missing_value", nodata_dp)
      call nc_var%setData(this%river%grid%unpack(this%previous_tributary))
    end if

    ! write muskingum parameters
    if ( allocated(this%nu1) ) then
      print*, "writing nu1 to restart_file"
      nc_var = restart_nc%setVariable("nu1", "f64", [xdim, ydim], deflate_level=deflate, shuffle=.true.)
      call nc_var%setAttribute("long_name", "muskingum parameter nu1")
      call nc_var%setFillValue(nodata_dp)
      call nc_var%setAttribute("missing_value", nodata_dp)
      call nc_var%setData(this%river%grid%unpack(this%nu1))
    end if

    if ( allocated(this%nu2) ) then
      print*, "writing nu2 to restart_file"
      nc_var = restart_nc%setVariable("nu2", "f64", [xdim, ydim], deflate_level=deflate, shuffle=.true.)
      call nc_var%setAttribute("long_name", "muskingum parameter nu2")
      call nc_var%setFillValue(nodata_dp)
      call nc_var%setAttribute("missing_value", nodata_dp)
      call nc_var%setData(this%river%grid%unpack(this%nu2))
    end if

  end subroutine river_router_write_restart_to_dataset

  !> \brief calculate the muskingum parameters nu1 and nu2
  subroutine river_router_setup_muskingum(this, max_route_step)
    use mo_utils, only: locate
    implicit none
    class(river_router_t), intent(inout) :: this
    real(dp), optional, intent(in) :: max_route_step !< [s] maximum routing time step (default: 86400.0)
    real(dp), allocatable :: k(:)
    real(dp) :: xi
    integer(i4) :: step_id

    xi = routing_space_weight ! fixed for now

    allocate(this%nu1(this%river%n_nodes), source=0.0_dp)
    allocate(this%nu2(this%river%n_nodes), source=0.0_dp)

    if (.not.allocated(this%river%link_length)) call error_message("river_router%setup_muskingum: link_length not available")
    if (.not.allocated(this%river%celerity)) call error_message("river_router%setup_muskingum: celerity not available")

    ! wave travel time parameter [s]
    allocate(k(this%river%n_nodes), source=(this%river%link_length / this%river%celerity))

    ! set min wave travel time to min routing step
    step_id = max(1_i4, locate(routing_steps, minval(k, mask=.not.this%river%is_sink)))
    this%step = routing_steps(step_id)
    if (present(max_route_step)) this%step = min(this%step, max_route_step)
    if (.not.any(equal(this%step, routing_steps))) call error_message("routine step is invalid: ", n2s(this%step))
    this%step = 3600.0_dp ! fix routing time step at 1h for now

    ! muskingum parameters
    this%nu1 = this%step / ( k * (1.0_dp - xi) + this%step / 2.0_dp )
    this%nu2 = 1.0_dp - this%nu1 * k / this%step

    ! output step size (at least input step) and number of iterations for sub-hour routing
    this%output_step = max(this%input_step, merge(1_i4, nint(this%step, i4) / HOUR_SECONDS, this%step < 3600.0_dp))
    this%iterations = (this%output_step * HOUR_SECONDS) / nint(this%step, i4) ! 1 if step > 3600
    ! if output step is longer than input, we need to accumulate inputs
    this%accumulations = this%output_step / this%input_step
  end subroutine river_router_setup_muskingum

  !> \brief Update routing for one time step
  subroutine river_router_update(this, input_runoff, discharge)
    implicit none
    class(river_router_t), intent(inout) :: this
    real(dp), dimension(this%input_grid%ncells), intent(in) :: input_runoff
    real(dp), dimension(this%river%n_nodes), intent(out) :: discharge
    integer(i4) :: i
    integer(i8) :: n, c
    ! accumulate input runoff
    this%input_count = this%input_count + 1_i4
    if (this%input_count == 1_i4) then
      this%acc_runoff = input_runoff ! first accumulation as runoff reset
    else
      this%acc_runoff = this%acc_runoff + input_runoff ! accumulate
    end if
    if (this%input_count < this%accumulations) return ! not yet ready to route
    ! reset input counter
    this%input_count = 0_i4
    ! average runoff flux over input time step accumulations
    if (this%accumulations > 1_i4) this%acc_runoff = this%acc_runoff / this%accumulations
    ! distribute runoff in case of SCC
    if (this%river%scc) then
      this%scaled_runoff = this%scale_runoff(this%acc_runoff)
      !$omp parallel do simd default(none) schedule(static) shared(this) private(c)
      do n = 1_i8, this%river%n_nodes
        c = this%river%node_cell(n)
        this%runoff(n) = this%scaled_runoff(c) * this%river%area_fraction(n)
      end do
      !$omp end parallel do simd
    else
      this%runoff = this%scale_runoff(this%acc_runoff)
    end if
    ! route
    do i = 1_i4, this%iterations
      call this%route(this%discharge, this%tributary)
      if (i == 1_i4) then
        discharge = this%discharge
      else
        discharge = discharge + this%discharge
      end if
      ! store discharge and tributary for next time-step
      call this%time_shift()
    end do
    ! average discharge flux over output time step iterations
    if (this%iterations > 1_i4) discharge = discharge / this%iterations
  end subroutine river_router_update

  !> \brief Setup river upscaler from fine river and coarse target grid.
  function river_router_scale_runoff(this, input_runoff) result(scaled_runoff_flux)
    implicit none
    class(river_router_t), intent(inout) :: this
    real(dp), dimension(this%input_grid%ncells), intent(in) :: input_runoff
    real(dp), dimension(this%river%grid%ncells) :: scaled_runoff_flux
    ! rescaled result in [liter] = [mm m2]
    if (this%scaler%scaling_mode == down_scaling) then
      call this%scaler%downscale_nearest(input_runoff, scaled_runoff_flux)
      scaled_runoff_flux = scaled_runoff_flux * this%river%grid%cell_area
    else
      call this%scaler%execute(input_runoff * this%input_grid%cell_area, scaled_runoff_flux)
    end if
    ! from [liter TS-1] to [m3 s-1]
    scaled_runoff_flux = scaled_runoff_flux * (liter_to_m3 / (this%input_step * HOUR_SECONDS))
  end function river_router_scale_runoff

  !> \brief Execute routing for a single routing time step.
  subroutine river_router_route(this, discharge, tributary)
    use mo_dag, only: node
    implicit none
    class(river_router_t), intent(in) :: this
    real(dp), dimension(this%river%n_nodes), intent(out) :: discharge, tributary
    integer(i8) :: i, j, n_levels
    logical, pointer, contiguous :: is_sink(:)
    integer(i8), pointer, contiguous :: id(:), level_start(:), level_end(:)

    ! pointers for speeding up dereferencing attributes (river is a pointer, so this works)
    id => this%river%order%id
    level_start => this%river%order%level_start
    level_end => this%river%order%level_end
    is_sink => this%river%is_sink
    n_levels = this%river%order%n_levels

    ! parallel routing on levels with size above threshold
    if (this%last_parallel_level > 0_i8) then
      !$omp parallel default(shared) private(i)
      do i = 1_i8, this%last_parallel_level
        !$omp do simd schedule(static)
        do j = level_start(i), level_end(i)
          call process_node(j)
        end do
        !$omp end do simd
      end do
      !$omp end parallel
    end if

    ! serial routing of nodes from remaining levels
    if (this%last_parallel_level == n_levels) return ! no node left
    do j = level_start(this%last_parallel_level + 1_i8), level_end(n_levels)
      call process_node(j)
    end do

  contains
    subroutine process_node(ni)
      integer(i8), intent(in) :: ni
      integer(i8) :: n
      integer(i4) :: n_edges, m
      integer(i8), pointer, contiguous :: edges(:)
      real(dp) :: inflow, prev_inflow, prev_routed
      n = id(ni)
      ! add runoff to inflow
      inflow = this%runoff(n)
      ! add upstream routed flux to inflow
      call this%river%src_view(n, edges)
      n_edges = size(edges)
      !$omp simd reduction(+:inflow)
      do m = 1_i4, n_edges
        inflow = inflow + tributary(edges(m))
      end do
      ! TODO: add/replace inflow with inflow handler at inflow gauges
      ! discharge is the accumulated inflow at current node
      discharge(n) = inflow
      if (is_sink(n)) then
        ! no routing at sinks
        tributary(n) = 0.0_dp
      else
        ! muskingum schema for routed flux
        prev_routed = this%previous_tributary(n)
        prev_inflow = this%previous_discharge(n)
        tributary(n) = prev_routed + this%nu1(n) * (prev_inflow - prev_routed) + this%nu2(n) * (inflow - prev_inflow)
      end if
    end subroutine process_node
  end subroutine river_router_route

  !> \brief Move current discharge and tributary to previous arrays.
  subroutine river_router_time_shift(this)
    implicit none
    class(river_router_t), intent(inout) :: this
    real(dp), allocatable :: tmp(:)
    ! swap discharge and previous_discharge
    call move_alloc(this%previous_discharge, tmp)
    call move_alloc(this%discharge, this%previous_discharge)
    call move_alloc(tmp, this%discharge)
    ! swap tributary and previous_tributary
    call move_alloc(this%previous_tributary, tmp)
    call move_alloc(this%tributary, this%previous_tributary)
    call move_alloc(tmp, this%tributary)
  end subroutine river_router_time_shift
end module mo_river_router
