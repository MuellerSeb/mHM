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
  use mo_grid_scaler, only: regridder, up_sum, down_nearest
  use mo_message, only: error_message

  implicit none
  private

  !> \name Routing Constants
  !> \brief Constants controlling the routing algorithms.
  !!@{
  !> valid routing time steps [s] (integer minute fractions of hour or integer hour fractions of day)
  real(dp), public, dimension(19), parameter :: routing_steps = &
    [   60.0_dp,   120.0_dp,   180.0_dp,   240.0_dp,   300.0_dp,   360.0_dp,           &
       600.0_dp,   720.0_dp,   900.0_dp,  1200.0_dp,  1800.0_dp,  3600.0_dp,           &
      7200.0_dp, 10800.0_dp, 14400.0_dp, 21600.0_dp, 28800.0_dp, 43200.0_dp, 86400.0_dp ]
  !> space weighting of routing is set to 0, since this parameter has no effect on the routing results, see Thober et al. 2017
  real(dp), public, parameter :: routing_space_weight = 0.0_dp
  !!@}

  !> \class inflow_t
  !> \brief Inflow handler
  type, public :: inflow_t
    integer(i8) :: count !< number of inflow nodes
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
    type(inflow_t) :: inflow !< inflow handler
    real(dp), allocatable :: c1(:) !< Muskingum parameter C1 size(river\%n_nodes)
    real(dp), allocatable :: c2(:) !< Muskingum parameter C2 size(river\%n_nodes)
  contains
    procedure, public :: init => river_router_init
  end type river_router_t

contains

  !> \brief Setup river upscaler from fine river and coarse target grid.
  subroutine river_router_init(this, river, input_grid)
    implicit none
    class(river_router_t), intent(inout) :: this
    type(river_t), pointer, intent(in) :: river !< river definition
    type(grid_t), pointer, intent(in) :: input_grid !< input grid
    this%river => river
    this%input_grid => input_grid
    call this%scaler%init( &
      source_grid=this%input_grid, &
      target_grid=this%river%grid, &
      upscaling_operator=up_sum, &       ! if L11 coarser than L1: sum runoff
      downscaling_operator=down_nearest) ! if L11 finer than L1: distribute same value on fine cells
  end subroutine river_router_init

  !> \brief calculate the muskingum parameters c1 and c2
  subroutine river_router_parameters(this, max_step)
    use mo_utils, only: locate
    implicit none
    class(river_router_t), intent(inout) :: this
    real(dp), optional, intent(in) :: max_step !< [s] maximum routing time step (default: 86400.0)
    real(dp), allocatable :: k(:)
    real(dp) :: xi, step, max_step_
    integer(i4) :: step_id

    xi = routing_space_weight ! fixed for now

    allocate(this%c1(this%river%n_nodes), source=0.0_dp)
    allocate(this%c2(this%river%n_nodes), source=0.0_dp)

    ! wave travel time parameter [s]
    allocate(k(this%river%n_nodes), source=(this%river%link_length / this%river%celerity))

    ! set min-wave traveltime to min routing step
    step_id = max(1_i4, locate(routing_steps, minval(k, mask=.not.this%river%is_sink)))
    step = routing_steps(step_id)
    if (present(max_step)) step = min(step, max_step)
    if (.not.any(equal(step, routing_steps))) call error_message("routine step is invalid: ", n2s(step))
    step = 3600.0_dp ! fix routing time step at 1h for now

    ! muskingum parameters
    this%c1 = step / ( k * (1.0_dp - xi) + step / 2.0_dp )
    this%c2 = 1.0_dp - this%c1 * k / step

  end subroutine river_router_parameters

end module mo_river_router
