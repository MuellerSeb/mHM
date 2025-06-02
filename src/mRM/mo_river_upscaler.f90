!> \file    mo_river_upscaler.f90
!> \copydoc mo_river_upscaler

!> \brief   River upscaler.
!> \details This module contains an upscaler for river networks.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    May 2025
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! This code is released under the LGPLv3+ license \license_note
module mo_river_upscaler

  use mo_kind, only: i4, i8, dp
  use mo_dag, only: traversal_visit
  use mo_river, only: river_t, dir_E, dir_S, dir_W, dir_N, dir_SE, dir_SW, dir_NW, dir_NE
  use mo_grid, only: grid_t, bottom_up
  use mo_grid_scaler, only: regridder, up_scaling
  use mo_message, only: error_message

  implicit none
  private

  !> \class river_upscaler_t
  !> \brief River network upscaler
  !> \details upscale river network respecting scc.
  !! Nodes are ordered in reverse sub-catchment order: first the nodes of the base catchment are added,
  !! then the nodes of the sub-catchment flowing into the base catchment and so on.
  type, public :: river_upscaler_t
    type(river_t), pointer :: fine_river !< river definition at fine grid
    type(river_t), pointer :: coarse_river !< river definition at coarse grid
    type(regridder) :: upscaler !< upscaler from fine to coarse grid
    ! coarse attributes derived from fine grid
    logical, allocatable :: leaving_cells(:) !< mask marking fine cells leaving a coarse cell size(fine\%ncells)
    logical, allocatable :: stream_mask(:) !< mask marking the upscaled stream at fine grid size(fine\%ncells)
    integer(i8), allocatable :: floodplain(:) !< map marking floodplain of a coarse river node with their id size(fine\%ncells)
    real(dp), allocatable :: floodplain_area(:) !< floodplain area for a coarse river node size(n_nodes)
    integer(i8), allocatable :: link_from(:) !< starting cell at fine grid of coarse river node (0 for sink) size(n_nodes)
    integer(i8), allocatable :: link_to(:) !< end cell at fine grid of coarse river node (0 for sink) size(n_nodes)
    ! scc related attributes
    integer(i4), allocatable :: scc_map(:) !< sub catchment id map (id 'nsub' indicates base-catchment)
    integer(i8), allocatable :: scc_gauges(:) !< id of gauge for sub catchment size(nsub-1)
    ! sub-catchment related attributes
    integer(i4) :: nsub = 1_i4 !< number of sub catchments
    logical, allocatable :: has_sub(:,:) !< flag if coarse cell contains node of a sub-catchment size(coarse\%ncells,nsub)
    integer(i4), allocatable :: n_sub_nodes(:) !< number of scc nodes per coarse grid cell size(coarse\%ncells)
    integer(i4), allocatable :: node_sub(:) !< map node to sub catchment id size(n_nodes)
    integer(i8), allocatable :: sub_size(:) !< number of coarse river nodes in each sub catchment size(nsub)
  contains
    procedure, public :: init => river_upscaler_init
  end type river_upscaler_t

contains
  !> \brief Setup river upscaler from fine river and coarse target grid.
  subroutine river_upscaler_init(this, fine_river, coarse_river, coarse_grid, scc_gauges, tol)
    implicit none
    class(river_upscaler_t), intent(inout) :: this
    type(river_t), pointer, intent(in) :: fine_river !< river definition at fine grid
    type(river_t), pointer, intent(in) :: coarse_river !< pointer to coarse river definition to be determined
    type(grid_t), pointer, intent(in) :: coarse_grid !< coarse grid for the upscaled river network
    integer(i4), dimension(:,:), intent(in), optional :: scc_gauges !< gauge locations at fine river dim 1: x/y, dim 2: gauge id
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparison (default: 1.e-7)
    integer(i8), allocatable :: cells(:,:)
    integer(i4), allocatable :: scc_map(:,:)
    integer(i8) :: i, k
    integer(i4) :: j, ix, iy
    integer(i4) :: yl, yu, xl, xu ! lower and upper bounds for x and y
    integer(i4) :: yn, ys ! north and south y-bound depending on grid y-direction

    this%fine_river => fine_river
    this%coarse_river => coarse_river
    this%coarse_river%grid => coarse_grid
    call this%upscaler%init(this%fine_river%grid, this%coarse_river%grid, tol=tol)
    if (this%upscaler%scaling_mode /= up_scaling) call error_message("river_upscaler: target grid needs to be coarser then input!")

    if (present(scc_gauges)) then
      call init_scc(this%fine_river, scc_gauges, this%nsub, this%scc_gauges, this%scc_map)
      this%coarse_river%scc = this%nsub > 1_i4
    else
      this%coarse_river%scc = .false.
      this%nsub = 1_i4
      allocate(this%scc_map(this%fine_river%n_nodes), source=1_i4)
      allocate(this%scc_gauges(0))
    end if

    ! find all leaving cells (fine cells at coarse cell borders pointing out the coarse cell)
    !------------------------------------------------------------------
    !                            xl     xu
    !                        yn  NW--N--NE       sides:   E, S, W, N
    !                            |       |       corners: SE,SW,NW,NE
    !                            W  (i)  E       yu and yl with inverse sky direction if grid is top-down -> yn and ys
    !                            |       |
    !                        ys  SW--S--SE
    !------------------------------------------------------------------
    cells = this%fine_river%grid%id_matrix()
    scc_map = this%fine_river%grid%unpack(this%scc_map)
    allocate(this%leaving_cells(this%fine_river%grid%ncells), source=.false.)
    if (this%coarse_river%scc) then
      allocate(this%n_sub_nodes(this%coarse_river%grid%ncells), source=0_i4)
      allocate(this%has_sub(this%coarse_river%grid%ncells, this%nsub), source=.false.)
    else
      allocate(this%n_sub_nodes(this%coarse_river%grid%ncells), source=1_i4)
      allocate(this%has_sub(this%coarse_river%grid%ncells, this%nsub), source=.true.)
    end if

    !$omp parallel do default(shared) private(i, yn, ys, ix, iy, j, yl, yu, xl, xu)
    do i = 1_i8, this%coarse_river%grid%ncells
      yl = this%upscaler%y_lb(i) ! lower y-bound
      yu = this%upscaler%y_ub(i) ! upper y-bound
      xl = this%upscaler%x_lb(i) ! lower x-bound
      xu = this%upscaler%x_ub(i) ! upper x-bound
      ! determine north and south bound depending on y-direction
      if (this%fine_river%grid%y_direction == bottom_up) then
        yn = yu ! north y-bound is up
        ys = yl ! south y-bound is down
      else ! top_down
        yn = yl ! north y-bound is down
        ys = yu ! south y-bound is up
      end if

      ! searching on side E
      do iy = yl + 1_i4, yu - 1_i4
        if (.not.this%fine_river%grid%mask(xu,iy)) cycle
        if (any(this%fine_river%fdir(cells(xu,iy))==[dir_NE, dir_E, dir_SE])) this%leaving_cells(cells(xu,iy)) = .true.
      end do
      ! searching on side S
      do ix = xl + 1_i4, xu - 1_i4
        if (.not.this%fine_river%grid%mask(ix,ys)) cycle
        if (any(this%fine_river%fdir(cells(ix,ys))==[dir_SE, dir_S, dir_SW])) this%leaving_cells(cells(ix,ys)) = .true.
      end do
      ! searching on side W
      do iy = yl + 1_i4, yu - 1_i4
        if (.not.this%fine_river%grid%mask(xl,iy)) cycle
        if (any(this%fine_river%fdir(cells(xl,iy))==[dir_NW, dir_W, dir_SW])) this%leaving_cells(cells(xl,iy)) = .true.
      end do
      ! searching on side N
      do ix = xl + 1_i4, xu - 1_i4
        if (.not.this%fine_river%grid%mask(ix,yn)) cycle
        if (any(this%fine_river%fdir(cells(ix,yn))==[dir_NE, dir_N, dir_NW])) this%leaving_cells(cells(ix,yn)) = .true.
      end do
      ! searching on corner SE
      if (this%fine_river%grid%mask(xu,ys)) then
        if (any(this%fine_river%fdir(cells(xu,ys))==[dir_NE,dir_E,dir_SE,dir_S,dir_SW])) this%leaving_cells(cells(xu,ys)) = .true.
      end if
      ! searching on corner SW
      if (this%fine_river%grid%mask(xl,ys)) then
        if (any(this%fine_river%fdir(cells(xl,ys))==[dir_NW,dir_W,dir_SW,dir_S,dir_SE])) this%leaving_cells(cells(xl,ys)) = .true.
      end if
      ! searching on corner NE
      if (this%fine_river%grid%mask(xu,yn)) then
        if (any(this%fine_river%fdir(cells(xu,yn))==[dir_NW,dir_N,dir_NE,dir_E,dir_SE])) this%leaving_cells(cells(xu,yn)) = .true.
      end if
      ! searching on corner NW
      if (this%fine_river%grid%mask(xl,yn)) then
        if (any(this%fine_river%fdir(cells(xl,yn))==[dir_SW,dir_W,dir_NW,dir_N,dir_NE])) this%leaving_cells(cells(xl,yn)) = .true.
      end if

      ! count number of scc nodes
      if (this%coarse_river%scc) then
        do j = 1_i4, this%nsub
          if (any(scc_map(xl:xu,yl:yu)==j)) this%has_sub(i, j) = .true.
        end do
      end if
    end do
    !$omp end parallel do

    ! determine attributes for scc
    if (this%coarse_river%scc) then
      ! coarse river attributes
      this%coarse_river%n_nodes = count(this%has_sub, kind=i8)
      allocate(this%coarse_river%node_cell(this%coarse_river%n_nodes))
      allocate(this%coarse_river%area_fraction(this%coarse_river%n_nodes))
      ! sub-catchment attributes
      this%sub_size = count(this%has_sub, dim=1, kind=i8)
      this%n_sub_nodes = count(this%has_sub, dim=2, kind=i4)
      allocate(this%node_sub(this%coarse_river%n_nodes))
      ! loop over sub-catchments
      !$omp parallel do default(shared) private(j, k, i)
      do j = 1_i4, this%nsub
        k = sum(this%sub_size(1_i4:j-1_i4)) ! count of nodes from previous sub-catchments
        do i = 1_i8, this%coarse_river%grid%ncells
          if (.not.this%has_sub(i, j)) cycle
          k = k + 1_i8
          this%coarse_river%node_cell(k) = i
          this%node_sub(k) = j
        end do
      end do
      !$omp end parallel do
      ! calcuate scc fractions in parallel
      !$omp parallel do default(shared) private(k, i)
      do k = 1_i8, this%coarse_river%n_nodes
        i = this%coarse_river%node_cell(k)
        this%coarse_river%area_fraction(k) = sum( &
          this%upscaler%weights(this%upscaler%x_lb(i):this%upscaler%x_ub(i),this%upscaler%y_lb(i):this%upscaler%y_ub(i)), &
          mask=(scc_map(this%upscaler%x_lb(i):this%upscaler%x_ub(i),this%upscaler%y_lb(i):this%upscaler%y_ub(i))==this%node_sub(k)))
      end do
      !$omp end parallel do
    else
      ! coarse river attributes
      allocate(this%coarse_river%area_fraction(this%coarse_river%n_nodes), source=1.0_dp)
      this%coarse_river%node_cell = [(i, i=1_i8, this%coarse_river%grid%ncells)] ! implicit allocation
      this%coarse_river%n_nodes = this%coarse_river%grid%ncells
      ! sub-catchment attributes
      allocate(this%sub_size(1), source=this%coarse_river%grid%ncells)
      allocate(this%n_sub_nodes(this%coarse_river%grid%ncells), source=1_i4)
      allocate(this%node_sub(this%coarse_river%n_nodes), source=1_i4)
    end if

    call this%coarse_river%init(this%coarse_river%n_nodes)
    ! TODO: determine edges of coarse river
    ! TODO: start with base catchment (nsub) -> determine coarse outlets

    deallocate(cells, scc_map)
  end subroutine river_upscaler_init

  !> \brief Generate SCC map
  subroutine init_scc(river, gauges, nsub, scc_gauges, scc_map)
    use mo_message, only: error_message
    class(river_t), intent(in) :: river !< river to derive sub-catchments from
    integer(i4), dimension(:,:), intent(in) :: gauges !< gauge locations dim 1: x/y, dim 2: gauge id
    integer(i4), intent(out) :: nsub !< number of sub catchments
    integer(i4), allocatable, intent(out) :: scc_map(:) !< sub catchment id (id 'nsub' indicates base-catchment)
    integer(i8), allocatable, intent(out) :: scc_gauges(:) !< node id of gauge for sub catchment size(nsub-1)
    ! sub-catchment related attributes
    logical, dimension(size(gauges, dim=2)) :: gauge_mask
    integer(i8), dimension(size(gauges, dim=2)) :: gauge_facc
    type(traversal_visit) :: handler
    integer(i4) :: i, j, n
    n = size(gauges, dim=2)
    nsub = n + 1_i4
    if (river%scc) call error_message("init_scc: need a D8 river to initialize SCC")
    if (.not.allocated(river%facc)) call error_message("init_scc: facc not initialized")
    allocate(scc_gauges(n))
    ! find gauge cell ids
    do i = 1_i4, n
      scc_gauges(i) = river%grid%cell_id(gauges(:,i))
      gauge_facc(i) = river%facc(scc_gauges(i))
    end do
    ! calculate scc map
    allocate(scc_map(river%grid%ncells), source=n+1_i4) ! base catchment gets id n+1
    allocate(handler%visited(river%grid%ncells))
    gauge_mask = .true.
    do i = 1_i4, n
      ! sort sub-gauges by facc (high to low)
      j = maxloc(gauge_facc, mask=gauge_mask, dim=1)
      gauge_mask(j) = .false.
      handler%visited = .false. ! reset traverse handler
      ! delineate catchment
      call river%traverse(handler, [scc_gauges(j)])
      where (handler%visited) scc_map = j
    end do
    deallocate(handler%visited)
  end subroutine init_scc

end module mo_river_upscaler
