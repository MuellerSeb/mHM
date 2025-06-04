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
    integer(i8), allocatable :: link_start(:) !< starting cell at fine grid of coarse river node (0 for sink) size(n_nodes)
    integer(i8), allocatable :: link_from(:) !< first cell after start at fine grid of coarse river node (0 for sink) size(n_nodes)
    integer(i8), allocatable :: link_to(:) !< end cell at fine grid of coarse river node (0 for sink) size(n_nodes)
    ! scc related attributes
    integer(i4), allocatable :: scc_map(:) !< sub catchment id map (id 'nsub' indicates base-catchment) size(fine\%n_nodes)
    integer(i8), allocatable :: scc_gauges(:) !< id of gauge for sub catchment on fine grid size(nsub-1)
    integer(i8), allocatable :: scc_coarse_gauges(:) !< id of gauge for sub catchment on coarse grid size(nsub-1)
    logical, allocatable :: is_scc_gauge(:) !< flag if node is a scc gauge size(fine%\n_nodes)
    logical, allocatable :: is_scc_coarse_gauge(:) !< flag if coarse node is a scc gauge size(coarse%\n_nodes)
    ! sub-catchment related attributes
    integer(i4) :: nsub = 1_i4 !< number of sub catchments
    logical, allocatable :: has_sub(:,:) !< flag if coarse cell contains node of a sub-catchment size(coarse\%ncells,nsub)
    integer(i4), allocatable :: n_sub_nodes(:) !< number of scc nodes per coarse grid cell size(coarse\%ncells)
    integer(i4), allocatable :: node_sub(:) !< map coarse node to sub catchment id size(coarse\%n_nodes)
    integer(i8), allocatable :: sub_size(:) !< number of coarse river nodes in each sub catchment size(nsub)
  contains
    procedure, public :: init => river_upscaler_init
    procedure, public :: init_scc => river_upscaler_init_scc
    procedure, public :: find_leaving_cells => river_upscaler_leaving
    procedure, public :: upscale_scc => river_upscaler_scc
    procedure, public :: upscale_fdir => river_upscaler_fdir
    procedure, public :: node_from_cell_sub => river_upscaler_cell_sub
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

    this%fine_river => fine_river
    this%coarse_river => coarse_river
    this%coarse_river%grid => coarse_grid
    call this%upscaler%init(this%fine_river%grid, this%coarse_river%grid, tol=tol)
    if (this%upscaler%scaling_mode /= up_scaling) call error_message("river_upscaler: target grid needs to be coarser then input!")

    ! initialize scc related variables
    call this%init_scc(scc_gauges)

    ! find leaving fine cells in every coarse cell
    call this%find_leaving_cells()

    ! upscale river depending on scc configuration
    if (this%coarse_river%scc) then
      call this%upscale_scc()
    else
      call this%upscale_fdir()
    end if
  end subroutine river_upscaler_init

  !> \brief Initialize SCC related variables
  subroutine river_upscaler_init_scc(this, scc_gauges)
    use mo_message, only: error_message
    class(river_upscaler_t), intent(inout) :: this
    integer(i4), dimension(:,:), intent(in), optional :: scc_gauges !< gauge locations at fine river dim 1: x/y, dim 2: gauge id
    ! sub-catchment related attributes
    logical, allocatable :: gauge_mask(:)
    integer(i8), allocatable :: gauge_facc(:)
    type(traversal_visit) :: handler
    integer(i4) :: i, j, n

    if (.not.present(scc_gauges)) then
      this%coarse_river%scc = .false.
      this%nsub = 1_i4
      allocate(this%scc_map(this%fine_river%n_nodes), source=1_i4)
      allocate(this%scc_gauges(0))
      allocate(this%scc_coarse_gauges(0))
      allocate(this%is_scc_gauge(this%fine_river%n_nodes), source=.false.)
      return
    end if

    n = size(scc_gauges, dim=2)
    allocate(gauge_mask(n))
    allocate(gauge_facc(n))
    this%nsub = n + 1_i4
    if (this%fine_river%scc) call error_message("init_scc: need a D8 river to initialize SCC")
    if (.not.allocated(this%fine_river%facc)) call error_message("init_scc: facc not initialized")
    allocate(this%scc_gauges(n))
    ! find gauge cell ids
    do i = 1_i4, n
      this%scc_gauges(i) = this%fine_river%grid%cell_id(scc_gauges(:,i))
      gauge_facc(i) = this%fine_river%facc(this%scc_gauges(i))
    end do
    ! calculate scc map
    allocate(this%scc_map(this%fine_river%grid%ncells), source=n+1_i4) ! base catchment gets id n+1
    allocate(handler%visited(this%fine_river%grid%ncells))
    gauge_mask = .true.
    do i = 1_i4, n
      ! sort sub-gauges by facc (high to low)
      j = maxloc(gauge_facc, mask=gauge_mask, dim=1)
      gauge_mask(j) = .false.
      handler%visited = .false. ! reset traverse handler
      ! delineate catchment
      call this%fine_river%traverse(handler, [this%scc_gauges(j)])
      where (handler%visited) this%scc_map = j
    end do
    deallocate(handler%visited)

    ! determine gauges
    allocate(this%is_scc_gauge(this%fine_river%grid%ncells), source=.false.)
    do i = 1_i4, n
      this%is_scc_gauge(this%scc_gauges(i)) = .true.
    end do
    this%coarse_river%scc = this%nsub > 1_i4
  end subroutine river_upscaler_init_scc

  !> \brief Find all leaving cells of fine grid on a coarse cell border.
  subroutine river_upscaler_leaving(this)
    implicit none
    class(river_upscaler_t), intent(inout) :: this
    integer(i8), allocatable :: cells(:,:)
    integer(i4), allocatable :: scc_map(:,:)
    integer(i8) :: i
    integer(i4) :: j, ix, iy
    integer(i4) :: yl, yu, xl, xu ! lower and upper bounds for x and y
    integer(i4) :: yn, ys ! north and south y-bound depending on grid y-direction

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
      allocate(this%has_sub(this%coarse_river%grid%ncells, this%nsub), source=.false.)
    else
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
  end subroutine river_upscaler_leaving

  !> \brief Setup coarse river with scc.
  subroutine river_upscaler_scc(this)
    implicit none
    class(river_upscaler_t), intent(inout) :: this
    integer(i8), allocatable :: facc(:,:), all_nodes(:)
    integer(i4), allocatable :: scc_map(:,:)
    logical, allocatable :: base_mask(:,:), dep_mask(:)
    integer(i8) :: i, k, node, next
    integer(i4) :: j, sub
    integer(i4) :: yl, yu, xl, xu ! lower and upper bounds for x and y

    if (.not.this%coarse_river%scc) call error_message("river_upscaler%scc: river is not a SCC river")

    ! coarse river attributes
    this%coarse_river%n_nodes = count(this%has_sub, kind=i8)
    allocate(this%coarse_river%node_cell(this%coarse_river%n_nodes))
    allocate(this%coarse_river%area_fraction(this%coarse_river%n_nodes))
    ! sub-catchment attributes
    this%sub_size = count(this%has_sub, dim=1, kind=i8)
    this%n_sub_nodes = count(this%has_sub, dim=2, kind=i4)
    allocate(this%node_sub(this%coarse_river%n_nodes))

    ! find scc gauge ids
    allocate(this%scc_coarse_gauges(size(this%scc_gauges)))
    allocate(this%is_scc_coarse_gauge(this%coarse_river%n_nodes), source=.false.)
    do j = 1_i4, size(this%scc_gauges)
      this%scc_coarse_gauges(j) = this%node_from_cell_sub(this%upscaler%id_map(this%scc_gauges(j)), j)
      this%is_scc_coarse_gauge(this%scc_coarse_gauges(j)) = .true.
    end do

    ! find containing cell and sub-catchment for each node
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
    scc_map = this%fine_river%grid%unpack(this%scc_map)
    !$omp parallel do default(shared) private(k, i)
    do k = 1_i8, this%coarse_river%n_nodes
      i = this%coarse_river%node_cell(k)
      this%coarse_river%area_fraction(k) = sum( &
        this%upscaler%weights(this%upscaler%x_lb(i):this%upscaler%x_ub(i),this%upscaler%y_lb(i):this%upscaler%y_ub(i)), &
        mask=(scc_map(this%upscaler%x_lb(i):this%upscaler%x_ub(i),this%upscaler%y_lb(i):this%upscaler%y_ub(i))==this%node_sub(k)))
    end do
    !$omp end parallel do
    deallocate(scc_map)

    call this%coarse_river%init(this%coarse_river%n_nodes)

    ! find outlets of of coarse grid
    facc = this%fine_river%grid%unpack(this%fine_river%facc)
    base_mask = this%fine_river%grid%unpack(this%scc_map==this%nsub)
    allocate(this%coarse_river%is_sink(this%coarse_river%n_nodes), source=.false.)
    !$omp parallel do default(shared) private(j, i, k, sub, yl, yu, xl, xu, node)
    do j = 1_i4, size(this%fine_river%sinks)
      i = this%fine_river%sinks(j) ! node ID is cell ID for a D8 river
      k = this%upscaler%id_map(i) ! coarse cell ID containing sink
      sub = this%scc_map(i) ! id of sub-catchment containing this sink
      ! in base-catchment, check if sink has max facc in respective coarse cell
      if (sub == this%nsub) then
        yl = this%upscaler%y_lb(k) ! lower y-bound
        yu = this%upscaler%y_ub(k) ! upper y-bound
        xl = this%upscaler%x_lb(k) ! lower x-bound
        xu = this%upscaler%x_ub(k) ! upper x-bound
        ! skip if facc is not max
        if (this%fine_river%facc(i) < maxval(facc(xl:xu,yl:yu), mask=base_mask(xl:xu,yl:yu))) cycle
      end if
      node = this%node_from_cell_sub(k, sub) ! get node ID (respecting scc)
      this%coarse_river%is_sink(node) = .true. ! mark coarse node as sink
    end do
    !$omp end parallel do
    deallocate(base_mask, facc)

    ! determine sinks
    allocate(this%coarse_river%sinks(count(this%coarse_river%is_sink)))
    k = 0_i8
    do i = 1_i8, this%coarse_river%n_nodes
      if (.not.this%coarse_river%is_sink(i)) cycle
      k = k + 1_i8
      this%coarse_river%sinks(k) = i
    end do

    ! init with 0 to indicate sinks
    allocate(this%link_start(this%coarse_river%n_nodes), source=0_i8)
    allocate(this%coarse_river%down(this%coarse_river%n_nodes), source=0_i8)

    ! construct links by finding coarse down-stream
    !$omp parallel do default(shared) private(i, k, sub, next)
    do i = 1_i8, this%coarse_river%n_nodes
      if (this%coarse_river%is_sink(i)) cycle ! skip sinks
      sub = this%node_sub(i) ! id of sub-catchment containing this node
      if (this%is_scc_coarse_gauge(i)) then
        this%link_start(i) = this%scc_gauges(sub)
      else ! leaving cell to next cell
        k = this%coarse_river%node_cell(i) ! coarse cell ID containing node
        ! location of leaving cell (could be optimized, but is memory efficient)
        this%link_start(i) = maxloc( &
          this%fine_river%facc, &
          mask=this%leaving_cells.and.(this%scc_map==sub).and.(this%upscaler%id_map==k), dim=1)
      end if
      next = this%fine_river%down(this%link_start(i))
      sub = this%scc_map(next) ! id of sub-catchment containing next node
      this%coarse_river%down(i) = this%node_from_cell_sub(this%upscaler%id_map(next), sub)
    end do
    !$omp end parallel do

    allocate(dep_mask(this%coarse_river%n_nodes))
    all_nodes = [(i, i=1_i8,this%coarse_river%n_nodes)]
    !$omp parallel do default(shared) private(i, dep_mask)
    do i = 1_i8, this%coarse_river%n_nodes
      dep_mask = this%coarse_river%down==i
      allocate(this%coarse_river%nodes(i)%edges(count(dep_mask)))
      this%coarse_river%nodes(i)%edges(:) = pack(all_nodes, mask=dep_mask)
      if (this%coarse_river%down(i) > 0_i8) then
        allocate(this%coarse_river%nodes(i)%dependents(1))
        this%coarse_river%nodes(i)%dependents(1) = this%coarse_river%down(i)
      end if
    end do
    !$omp end parallel do
    deallocate(dep_mask, all_nodes)
  end subroutine river_upscaler_scc

  !> \brief Setup stream features.
  subroutine river_upscaler_stream(this)
    implicit none
    class(river_upscaler_t), intent(inout) :: this
    integer(i8), allocatable :: facc(:,:), all_nodes(:)
    integer(i4), allocatable :: scc_map(:,:)
    logical, allocatable :: base_mask(:,:), dep_mask(:)
    integer(i8) :: i, k, node, next
    integer(i4) :: j, sub
    integer(i4) :: yl, yu, xl, xu ! lower and upper bounds for x and y

  end subroutine river_upscaler_stream

  !> \brief Setup coarse river from upscaled D8 fdir.
  subroutine river_upscaler_fdir(this)
    implicit none
    class(river_upscaler_t), intent(inout) :: this
    integer(i8), allocatable :: cells(:,:), facc(:,:)
    integer(i4), allocatable :: fdir(:)
    logical, allocatable :: leaving(:,:)
    integer(i8) :: i, k
    integer(i4) :: dir, loc(2), j, ix, iy, yl, yu, xl, xu ! lower and upper bounds for x and y
    integer(i4) :: yn, ys ! north and south y-bound depending on grid y-direction

    if (this%coarse_river%scc) call error_message("river_upscaler%d8: river is not a D8 river")

    ! sub-catchment attributes when scc is turned of
    allocate(this%sub_size(1), source=this%coarse_river%grid%ncells)
    allocate(this%n_sub_nodes(this%coarse_river%grid%ncells), source=1_i4)
    allocate(this%node_sub(this%coarse_river%grid%ncells), source=1_i4)

    ! construct coarse fdir to initialize coarse river
    cells = this%fine_river%grid%id_matrix()
    facc = this%fine_river%grid%unpack(this%fine_river%facc)
    leaving = this%fine_river%grid%unpack(this%leaving_cells)

    ! find outlets of of coarse grid
    allocate(fdir(this%coarse_river%grid%ncells), source=-1_i4) ! sinks to be determined
    !$omp parallel do default(shared) private(j, i, k, yl, yu, xl, xu)
    do j = 1_i4, size(this%fine_river%sinks)
      k = this%fine_river%sinks(j)
      i = this%upscaler%id_map(k) ! coarse cell ID containing sink
      yl = this%upscaler%y_lb(i) ! lower y-bound
      yu = this%upscaler%y_ub(i) ! upper y-bound
      xl = this%upscaler%x_lb(i) ! lower x-bound
      xu = this%upscaler%x_ub(i) ! upper x-bound
      ! skip if facc is not max
      if (this%fine_river%facc(k) < maxval(facc(xl:xu,yl:yu), mask=this%fine_river%grid%mask(xl:xu,yl:yu))) cycle
      fdir(i) = 0_i4 ! mark coarse node as sink
    end do
    !$omp end parallel do

    ! init with 0 to indicate sinks
    allocate(this%link_start(this%coarse_river%grid%ncells), source=0_i8)
    ! construct links by finding coarse down-stream
    !$omp parallel do default(shared) private(i, yl, yu, xl, xu, yn, ys, ix, iy, loc, dir)
    do i = 1_i8, this%coarse_river%grid%ncells
      if (fdir(i)==0_i4) cycle ! skip sinks
      yl = this%upscaler%y_lb(i) ! lower y-bound
      yu = this%upscaler%y_ub(i) ! upper y-bound
      xl = this%upscaler%x_lb(i) ! lower x-bound
      xu = this%upscaler%x_ub(i) ! upper x-bound
      if (this%fine_river%grid%y_direction == bottom_up) then
        yn = yu ! north y-bound is up
        ys = yl ! south y-bound is down
      else ! top_down
        yn = yl ! north y-bound is down
        ys = yu ! south y-bound is up
      end if
      loc = maxloc(facc(xl:xu,yl:yu), mask=leaving(xl:xu,yl:yu))
      ix = xl + loc(1) - 1_i4
      iy = yl + loc(2) - 1_i4
      this%link_start(i) = cells(ix,iy)
      dir = this%fine_river%fdir(this%link_start(i))
      if (ix==xu.and.iy==yn) then ! NE
        select case(dir)
          case(dir_NW, dir_N)
            fdir(i) = dir_N
          case(dir_NE)
            fdir(i) = dir_NE
          case(dir_E, dir_SE)
            fdir(i) = dir_E
        end select
      else if (ix==xu.and.iy==ys) then ! SE
        select case(dir)
          case(dir_NE, dir_E)
            fdir(i) = dir_E
          case(dir_SE)
            fdir(i) = dir_SE
          case(dir_S, dir_SW)
            fdir(i) = dir_S
        end select
      else if (ix==xl.and.iy==ys) then ! SW
        select case(dir)
          case(dir_SE, dir_S)
            fdir(i) = dir_S
          case(dir_SW)
            fdir(i) = dir_SW
          case(dir_W, dir_NW)
            fdir(i) = dir_W
        end select
      else if (ix==xl.and.iy==yn) then ! NW
        select case(dir)
          case(dir_SW, dir_W)
            fdir(i) = dir_W
          case(dir_NW)
            fdir(i) = dir_NW
          case(dir_N, dir_NE)
            fdir(i) = dir_N
        end select
      else if (ix==xu) then ! E
        fdir(i) = dir_E
      else if (ix==xl) then ! W
        fdir(i) = dir_W
      else if (iy==yn) then ! N
        fdir(i) = dir_N
      else if (iy==ys) then ! S
        fdir(i) = dir_S
      end if
    end do
    !$omp end parallel do
    deallocate(cells, facc, leaving)

    ! init coarse river from upscaled fdir
    call this%coarse_river%from_fdir(fdir)

    ! cleanup
    deallocate(fdir)
  end subroutine river_upscaler_fdir

  !> \brief determine node ID from coarse cell and sub-catchment id
  !> \details Nodes are ordered by sub-catchments:
  !! 1. all coarse cells touching sub-catchment 1
  !! 2. all coarse cells touching sub-catchment 2 ...
  !!
  !! last: all cells from the base-catchment (not touched by any specified sub-catchment)
  pure function river_upscaler_cell_sub(this, cell, sub) result(id)
    implicit none
    class(river_upscaler_t), intent(in) :: this
    integer(i8), intent(in) :: cell !< cell id on coarse river grid (1..coarse\%ncells)
    integer(i4), intent(in) :: sub !< sub-catchment id from scc (1..nsub)
    integer(i8) :: id
    if (.not.this%has_sub(cell, sub)) then
      ! given cell doesn't contain the specified sub-catchment
      id = 0_i8
      return
    end if
    id = sum(this%sub_size(1_i4:sub-1_i4)) ! count of nodes from previous sub-catchments
    id = id + count(this%has_sub(1_i8:cell, sub), kind=i8)
  end function river_upscaler_cell_sub
end module mo_river_upscaler
