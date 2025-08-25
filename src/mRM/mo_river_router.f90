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
  use mo_constants, only: nodata_i4
  use mo_utils, only: equal
  use mo_string_utils, only: n2s => num2str
  use mo_river, only: river_t
  use mo_grid, only: grid_t, bottom_up
  use mo_grid_scaler, only: regridder, up_sum, down_nearest, up_scaling
  use mo_message, only: error_message
  use mo_datetime, only: HOUR_SECONDS

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
    type(regridder) :: scaler !< input rescaler
    integer(i4) :: input_step !< [h] time step size of the input (either 1 or 24)
    integer(i4) :: input_count !< [-] number of accumulated inputs for longer output step
    integer(i4) :: output_step !< [h] time step size of the output (at least 1)
    real(dp) :: step !< [s] time step size of the routing (for meeting the CFL condition)
    integer(i4) :: iterations !< number of routing iterations for each output step
    integer(i4) :: accumulations !< number of input accumulations for each output step
    type(inflow_t) :: inflow !< inflow handler
    !> [m3 s-1] runoff flux to route for current node, size(river\%n_nodes)
    real(dp), allocatable :: runoff(:)
    !> [m3 s-1] inflow flux to link starting at current node (current and previous time step), size(river\%n_nodes)
    real(dp), allocatable :: link_inflow(:, :)
    !> [m3 s-1] routed flux from link starting at current node (current and previous time step), size(river\%n_nodes)
    real(dp), allocatable :: link_routed(:, :)
    real(dp), allocatable :: nu1(:) !< Muskingum parameter nu1, size(river\%n_nodes) -> C1 = nu2, C2 = nu1-nu2, C3=1-nu1
    real(dp), allocatable :: nu2(:) !< Muskingum parameter nu2, size(river\%n_nodes) -> C1 = nu2, C2 = nu1-nu2, C3=1-nu1
  contains
    procedure, public :: init => river_router_init
    procedure, public :: update => river_router_update
    procedure, private :: setup_muskingum => river_router_setup_muskingum
    procedure, private :: scale_runoff => river_router_scale_runoff
    procedure, private :: route => river_router_route
  end type river_router_t

contains

  !> \brief Setup river upscaler from fine river and coarse target grid.
  subroutine river_router_init(this, river, input_grid, input_step, inflow, max_route_step)
    implicit none
    class(river_router_t), intent(inout) :: this
    type(river_t), pointer, intent(in) :: river !< river definition
    type(grid_t), pointer, intent(in) :: input_grid !< input grid
    integer(i4), intent(in), optional :: input_step !< [h] input time step size (1 by default)
    type(inflow_t), intent(in), optional :: inflow !< inflow specifications
    real(dp), optional, intent(in) :: max_route_step !< [s] maximum routing time step (default: 86400.0)
    this%river => river
    this%input_grid => input_grid
    call this%scaler%init( &
      source_grid=this%input_grid, &
      target_grid=this%river%grid, &
      upscaling_operator=up_sum, &       ! if L11 coarser than L1: sum runoff
      downscaling_operator=down_nearest) ! if L11 finer than L1: distribute same value on fine cells
    if (present(inflow)) this%inflow = inflow
    this%input_count = 0_i4
    this%input_step = 1_i4
    if (present(input_step)) this%input_step = input_step
    allocate(this%runoff(this%river%n_nodes), source=0.0_dp)
    allocate(this%link_inflow(this%river%n_nodes, 2), source=0.0_dp)
    allocate(this%link_routed(this%river%n_nodes, 2), source=0.0_dp)
    ! setup muskingum parameters
    call this%setup_muskingum(max_route_step)
  end subroutine river_router_init

  !> \brief calculate the muskingum parameters nu1 and nu2
  subroutine river_router_setup_muskingum(this, max_route_step)
    use mo_utils, only: locate
    implicit none
    class(river_router_t), intent(inout) :: this
    real(dp), optional, intent(in) :: max_route_step !< [s] maximum routing time step (default: 86400.0)
    real(dp), allocatable :: k(:)
    real(dp) :: xi, max_step_
    integer(i4) :: step_id

    xi = routing_space_weight ! fixed for now

    allocate(this%nu1(this%river%n_nodes), source=0.0_dp)
    allocate(this%nu2(this%river%n_nodes), source=0.0_dp)

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
    ! TODO: accumulate before rescaling
    ! accumulate input runoff
    this%input_count = this%input_count + 1_i4
    if (this%input_count == 1_i4) then
      this%runoff = this%scale_runoff(input_runoff) ! first accumulation as runoff reset
    else
      this%runoff = this%runoff + this%scale_runoff(input_runoff) ! accumulate
    end if
    if (this%input_count < this%accumulations) return ! not yet ready to route
    ! average runoff flux over input time step accumulations
    if (this%accumulations > 1_i4) this%runoff = this%runoff / real(this%accumulations, dp)
    ! reset input counter
    this%input_count = 0_i4
    ! prepare discharge
    discharge = 0.0_dp
    ! route
    do i = 1_i4, this%iterations
      call this%route(discharge)
    end do
    ! average discharge flux over output time step iterations
    if (this%iterations > 1_i4) discharge = discharge / real(this%iterations, dp)
  end subroutine river_router_update

  !> \brief Setup river upscaler from fine river and coarse target grid.
  function river_router_scale_runoff(this, input_runoff) result(scaled_runoff_flux)
    implicit none
    class(river_router_t), intent(inout) :: this
    real(dp), dimension(this%input_grid%ncells), intent(in) :: input_runoff
    real(dp), dimension(this%river%grid%ncells) :: scaled_runoff_flux
    ! TODO: this could be optimized, since rescaler always unpacks the input
    ! rescaled result in [liter] = [mm m2]
    if (this%scaler%scaling_mode == up_scaling) then
      call this%scaler%execute(input_runoff * this%input_grid%cell_area, scaled_runoff_flux)
    else
      call this%scaler%execute(input_runoff, scaled_runoff_flux)
      scaled_runoff_flux = scaled_runoff_flux * this%river%grid%cell_area
    end if
    ! from [liter TS-1] to [m3 s-1]
    scaled_runoff_flux = scaled_runoff_flux * (liter_to_m3 / (this%input_step * HOUR_SECONDS))
  end function river_router_scale_runoff

  !> \brief Execute routing for a single routing time step.
  subroutine river_router_route(this, discharge)
    implicit none
    class(river_router_t), intent(inout) :: this
    real(dp), dimension(this%river%n_nodes), intent(inout) :: discharge
    integer(i8) :: i, j, n, m
    if (.not.allocated(this%river%order%id)) call error_message("router%route: river order not initialized")
    ! parallel routing on levels
    do i = 1_i8, size(this%river%order%level_start, kind=i8)
      !$omp parallel do default(shared) private(j, n, m)
      do j = this%river%order%level_start(i), this%river%order%level_end(i)
        n = this%river%order%id(j)
        ! add runoff to inflow
        this%link_inflow(n, current) = this%runoff(n)
        ! add upstream routed flux to inflow
        do m = 1, this%river%nodes(n)%nedges()
          this%link_inflow(n, current) = this%link_inflow(n, current) + this%link_routed(this%river%nodes(n)%edges(m), current)
        end do
        ! TODO: add/replace inflow with inflow handler at inflow gauges
        ! discharge is the accumulated inflow at current node
        discharge(n) = discharge(n) + this%link_inflow(n, current)
        ! no routing at sinks
        if (this%river%is_sink(n)) cycle
        ! muskingum schema for routed flux
        this%link_routed(n, current) = this%link_routed(n, previous) &
                                     + this%nu1(n) * (this%link_inflow(n, previous) - this%link_routed(n, previous)) &
                                     + this%nu2(n) * (this%link_inflow(n, current)  - this%link_inflow(n, previous))
      end do
      !$omp end parallel do
    end do
    ! move values from t -> t-1 for next iteration
    ! TODO: prevent copying by only swapping indices
    this%link_inflow(:, previous) = this%link_inflow(:, current)
    this%link_routed(:, previous) = this%link_routed(:, current)
  end subroutine river_router_route

end module mo_river_router
