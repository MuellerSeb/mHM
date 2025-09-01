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
  use mo_constants, only: nodata_i4
  use mo_dag, only: traversal_visit
  use mo_river, only: river_t, d8_E, d8_S, d8_W, d8_N, d8_SE, d8_SW, d8_NW, d8_NE
  use mo_grid, only: grid_t, bottom_up
  use mo_grid_scaler, only: scaler_t, down_scaling
  use mo_message, only: error_message

  implicit none
  private

  !> \class river_upscaler_t
  !> \brief River network upscaler
  !> \details upscale river network respecting scc.
  !! Nodes are ordered in reverse sub-catchment order: first the nodes of the base catchment are added,
  !! then the nodes of the sub-catchment flowing into the base catchment and so on.
  type, public :: river_upscaler_t
    type(river_t), pointer :: fine_river => null() !< river definition at fine grid
    type(river_t), pointer :: coarse_river => null() !< river definition at coarse grid
    type(scaler_t) :: upscaler !< upscaler from fine to coarse grid
    ! coarse attributes derived from fine grid
    logical, allocatable :: leaving_cells(:) !< mask marking fine cells leaving a coarse cell size(fine\%ncells)
    logical, allocatable :: stream_mask(:) !< mask marking the upscaled stream at fine grid size(fine\%ncells)
    integer(i4), allocatable :: stream_sub(:) !< sub-catchment ID for stream cell at fine grid size(fine\%ncells)
    integer(i8), allocatable :: floodplain(:) !< map marking floodplain of a coarse river node with their id size(fine\%ncells)
    real(dp), allocatable :: floodplain_area(:) !< floodplain area for a coarse river node size(coarse\%n_nodes)
    logical, allocatable :: is_link_start(:) !< mask starting cell at fine grid of coarse river node size(fine\%n_nodes)
    integer(i8), allocatable :: link_start(:) !< starting cell at fine grid of coarse river node (0 for sink) size(coarse\%n_nodes)
    integer(i8), allocatable :: link_from(:) !< first fine cell after start for coarse river node (0 for sink) size(coarse\%n_nodes)
    integer(i8), allocatable :: link_to(:) !< end cell at fine grid of coarse river node (0 for sink) size(coarse\%n_nodes)
    integer(i8), allocatable :: sink_map(:) !< id of defining fine cell for a coarse sink (0 for no sink) size(coarse\%n_nodes)
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
    procedure, private :: init_scc => river_upscaler_init_scc
    procedure, private :: find_leaving_cells => river_upscaler_leaving
    procedure, private :: upscale_scc => river_upscaler_scc
    procedure, private :: upscale_fdir => river_upscaler_fdir
    procedure, private :: calc_stream => river_upscaler_stream
    procedure, public :: calc_celerity => river_upscaler_celerity
    procedure, public :: node_from_cell_sub => river_upscaler_cell_sub
  end type river_upscaler_t

contains

  !> \brief Setup river upscaler from fine river and coarse target grid.
  subroutine river_upscaler_init(this, fine_river, coarse_river, coarse_grid, scc_gauges, calc_stream, tol)
    implicit none
    class(river_upscaler_t), intent(inout) :: this
    type(river_t), pointer, intent(in) :: fine_river !< river definition at fine grid
    type(river_t), pointer, intent(in) :: coarse_river !< pointer to coarse river definition to be determined
    type(grid_t), pointer, intent(in) :: coarse_grid !< coarse grid for the upscaled river network
    integer(i4), dimension(:,:), intent(in), optional :: scc_gauges !< gauge locations at fine river dim 1: x/y, dim 2: gauge id
    logical, optional, intent(in) :: calc_stream !< Whether to calculate stream features (default: .true.)
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparison (default: 1.e-7)
    logical :: stream = .true.
    if (present(calc_stream)) stream = calc_stream

    this%fine_river => fine_river
    this%coarse_river => coarse_river
    this%coarse_river%grid => coarse_grid
    call this%upscaler%init(this%fine_river%grid, this%coarse_river%grid, tol=tol)
    if (this%upscaler%scaling_mode == down_scaling) call error_message("river_upscaler: target grid needs to be coarser then input")
    ! TODO: shortcut for same resolution of fine and coarse river

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

    ! order the coarse river
    call this%coarse_river%calc_order()

    ! calculate stream features (stream mask, link lengths)
    if (stream) call this%calc_stream()

  end subroutine river_upscaler_init

  !> \brief Initialize SCC related variables
  subroutine river_upscaler_init_scc(this, scc_gauges)
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
    if (this%fine_river%scc) call error_message("river_upscaler%init_scc: need a D8 river to initialize SCC")
    if (.not.allocated(this%fine_river%facc)) call error_message("river_upscaler%init_scc: facc not available")
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
    allocate(this%leaving_cells(this%fine_river%grid%ncells), source=.false.)

    if (this%coarse_river%scc) then
      scc_map = this%fine_river%grid%unpack(this%scc_map)
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
        yn = yu ! north y-bound is upper bound
        ys = yl ! south y-bound is lower bound
      else ! top_down
        yn = yl ! north y-bound is lower bound
        ys = yu ! south y-bound is upper bound
      end if

      ! searching on side E
      do iy = yl + 1_i4, yu - 1_i4
        if (.not.this%fine_river%grid%mask(xu,iy)) cycle
        if (any(this%fine_river%fdir(cells(xu,iy))==[d8_NE, d8_E, d8_SE])) this%leaving_cells(cells(xu,iy)) = .true.
      end do
      ! searching on side S
      do ix = xl + 1_i4, xu - 1_i4
        if (.not.this%fine_river%grid%mask(ix,ys)) cycle
        if (any(this%fine_river%fdir(cells(ix,ys))==[d8_SE, d8_S, d8_SW])) this%leaving_cells(cells(ix,ys)) = .true.
      end do
      ! searching on side W
      do iy = yl + 1_i4, yu - 1_i4
        if (.not.this%fine_river%grid%mask(xl,iy)) cycle
        if (any(this%fine_river%fdir(cells(xl,iy))==[d8_NW, d8_W, d8_SW])) this%leaving_cells(cells(xl,iy)) = .true.
      end do
      ! searching on side N
      do ix = xl + 1_i4, xu - 1_i4
        if (.not.this%fine_river%grid%mask(ix,yn)) cycle
        if (any(this%fine_river%fdir(cells(ix,yn))==[d8_NE, d8_N, d8_NW])) this%leaving_cells(cells(ix,yn)) = .true.
      end do
      ! searching on corner SE
      if (this%fine_river%grid%mask(xu,ys)) then
        if (any(this%fine_river%fdir(cells(xu,ys))==[d8_NE,d8_E,d8_SE,d8_S,d8_SW])) this%leaving_cells(cells(xu,ys)) = .true.
      end if
      ! searching on corner SW
      if (this%fine_river%grid%mask(xl,ys)) then
        if (any(this%fine_river%fdir(cells(xl,ys))==[d8_NW,d8_W,d8_SW,d8_S,d8_SE])) this%leaving_cells(cells(xl,ys)) = .true.
      end if
      ! searching on corner NE
      if (this%fine_river%grid%mask(xu,yn)) then
        if (any(this%fine_river%fdir(cells(xu,yn))==[d8_NW,d8_N,d8_NE,d8_E,d8_SE])) this%leaving_cells(cells(xu,yn)) = .true.
      end if
      ! searching on corner NW
      if (this%fine_river%grid%mask(xl,yn)) then
        if (any(this%fine_river%fdir(cells(xl,yn))==[d8_SW,d8_W,d8_NW,d8_N,d8_NE])) this%leaving_cells(cells(xl,yn)) = .true.
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

    if (.not.this%coarse_river%scc) call error_message("river_upscaler%upscale_scc: river is not a SCC river")

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
    allocate(this%sink_map(this%coarse_river%n_nodes), source=0_i8)
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
      this%sink_map(node) = i ! store corresponding fine sink
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

    if (this%coarse_river%scc) call error_message("river_upscaler%upscale_fdir: river is not a D8 river")
    if (.not.allocated(this%fine_river%facc)) call error_message("river_upscaler%upscale_fdir: facc not available")

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
    allocate(this%sink_map(this%coarse_river%grid%ncells), source=0_i8)
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
      this%sink_map(i) = k ! store corresponding fine sink
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
          case(d8_NW, d8_N)
            fdir(i) = d8_N
          case(d8_NE)
            fdir(i) = d8_NE
          case(d8_E, d8_SE)
            fdir(i) = d8_E
        end select
      else if (ix==xu.and.iy==ys) then ! SE
        select case(dir)
          case(d8_NE, d8_E)
            fdir(i) = d8_E
          case(d8_SE)
            fdir(i) = d8_SE
          case(d8_S, d8_SW)
            fdir(i) = d8_S
        end select
      else if (ix==xl.and.iy==ys) then ! SW
        select case(dir)
          case(d8_SE, d8_S)
            fdir(i) = d8_S
          case(d8_SW)
            fdir(i) = d8_SW
          case(d8_W, d8_NW)
            fdir(i) = d8_W
        end select
      else if (ix==xl.and.iy==yn) then ! NW
        select case(dir)
          case(d8_SW, d8_W)
            fdir(i) = d8_W
          case(d8_NW)
            fdir(i) = d8_NW
          case(d8_N, d8_NE)
            fdir(i) = d8_N
        end select
      else if (ix==xu) then ! E
        fdir(i) = d8_E
      else if (ix==xl) then ! W
        fdir(i) = d8_W
      else if (iy==yn) then ! N
        fdir(i) = d8_N
      else if (iy==ys) then ! S
        fdir(i) = d8_S
      end if
    end do
    !$omp end parallel do
    deallocate(cells, facc, leaving)

    ! init coarse river from upscaled fdir (length will be set later)
    call this%coarse_river%from_fdir(fdir, calculate_length=.false.)

    ! cleanup
    deallocate(fdir)
  end subroutine river_upscaler_fdir

  !> \brief Setup stream features.
  subroutine river_upscaler_stream(this)
    implicit none
    class(river_upscaler_t), intent(inout) :: this
    integer(i8) :: i, cell

    if (.not.allocated(this%fine_river%link_length)) call error_message("river_upscaler%calc_stream: link length not available")

    ! create is_link_start array
    allocate(this%is_link_start(this%fine_river%n_nodes), source=.false.)
    !$omp parallel do default(shared) private(i)
    do i = 1_i8, this%coarse_river%n_nodes
      if (this%coarse_river%is_sink(i)) cycle ! skip sinks
      this%is_link_start(this%link_start(i)) = .true.
    end do
    !$omp end parallel do

    allocate(this%coarse_river%link_length(this%coarse_river%n_nodes), source=0.0_dp)
    allocate(this%stream_mask(this%fine_river%n_nodes), source=.false.)
    allocate(this%stream_sub(this%fine_river%n_nodes), source=nodata_i4)
    allocate(this%link_from(this%coarse_river%n_nodes), source=0_i8)
    allocate(this%link_to(this%coarse_river%n_nodes), source=0_i8)

    !$omp parallel do default(shared) private(i, cell)
    do i = 1_i8, this%coarse_river%n_nodes
      if (this%coarse_river%is_sink(i)) then
        this%stream_mask(this%sink_map(i)) = .true. ! mark sinks as part of stream
        cycle ! skip sinks
      end if
      cell = this%link_start(i)
      ! TODO: link_from could be simply link_start
      this%link_from(i) = this%fine_river%down(cell)
      ! go down the river
      do
        ! this can cause a "benign race condition" with OpenMP, but since it is only flipping to true, outcome does not change
        !!$omp atomic write
        this%stream_mask(cell) = .true.
        ! add up river link length
        this%coarse_river%link_length(i) = this%coarse_river%link_length(i) + this%fine_river%link_length(cell)
        cell = this%fine_river%down(cell)
        ! TODO: decide if ending at leaving cell or another link-start of a coarse grid cell (legacy behavior)
        if (this%leaving_cells(cell)) exit ! end at leaving cell (every link-start is a leaving cell)
        ! if (this%is_link_start(cell)) exit ! end at another link-start
        if (this%is_scc_gauge(cell)) exit ! end at scc gauge
        if (this%fine_river%is_sink(cell)) exit ! end at fine sink
      end do
      ! mask the to-node as well (again "benign race condition")
      this%stream_mask(cell) = .true.
      this%link_to(i) = cell
    end do
    !$omp end parallel do

    ! generate stream sub-catchment map from mask
    where(this%stream_mask) this%stream_sub = this%scc_map

  end subroutine river_upscaler_stream

  !> \brief calculate the celerity c_i from slope s_i (i - cell index)
  subroutine river_upscaler_celerity(this, gamma, constant_celerity, slope)
    use mo_mad, only: mad
    use mo_utils, only: locate
    implicit none
    class(river_upscaler_t), intent(inout) :: this
    real(dp), intent(in) :: gamma !< model parameter: c_i = gamma * sqrt(s_i) or c = gamma
    logical, optional, intent(in) :: constant_celerity !< whether celerity is assumed constant: c = gamma (default: .false.)
    real(dp), optional, intent(in) :: slope(this%fine_river%n_nodes) !< [%] river slope on fine grid: size(fine\%ncells)
    real(dp), allocatable :: smooth_slope(:)
    integer(i8) :: i, cell
    real(dp) :: n

    logical :: constant = .false.
    if (present(constant_celerity)) constant = constant_celerity

    if (constant) then
      allocate(this%fine_river%celerity(this%fine_river%n_nodes), source=gamma)
      allocate(this%coarse_river%celerity(this%coarse_river%n_nodes), source=gamma)
      return
    end if

    if (.not.present(slope)) call error_message("river_upscaler%calc_celerity: need slope on fine grid.")
    allocate(smooth_slope(this%fine_river%n_nodes), source=slope)
    ! set min val for river slope to enable h-mean
    where ( smooth_slope < 0.1_dp ) smooth_slope = 0.1_dp
    ! smooth river slope if there is more than one cell
    if( count(this%stream_mask) > 1) smooth_slope = mad(arr=smooth_slope, z=2.25_dp, mask=this%stream_mask, tout="u", mval=0.1_dp)

    allocate(this%fine_river%celerity(this%fine_river%n_nodes), source=(gamma * sqrt(smooth_slope / 100.0_dp)))
    allocate(this%coarse_river%celerity(this%coarse_river%n_nodes), source=1.0_dp)

    !$omp parallel do default(shared) private(i, cell, n)
    do i = 1_i8, this%coarse_river%n_nodes
      if (this%coarse_river%is_sink(i)) cycle ! this also catches one-cell setups
      ! TODO: link_from could be simply link_start
      cell = this%link_from(i)
      this%coarse_river%celerity(i) = 0.0_dp
      n = 0.0_dp
      ! one pass algorithm for harmonic mean:
      ! 0. M  = 0               -> 0 as initial value for the [M]ean of inverses
      ! 1. M += (1/v - M) / n   -> updating the mean with the weighted deviation of new value (n as counter)
      ! 2. H  = 1 / M           -> [H]armonic mean is then the inverse of M
      do ! go down the river
        n = n + 1.0_dp
        this%coarse_river%celerity(i) = this%coarse_river%celerity(i) &
          + ( 1.0_dp / this%fine_river%celerity(cell) - this%coarse_river%celerity(i) ) / n
        ! exit at end of link
        if (this%link_to(i) == cell) exit
        cell = this%fine_river%down(cell)
      end do
      ! finalize harmonic mean for celerity
      this%coarse_river%celerity(i) = 1.0_dp / this%coarse_river%celerity(i)
    end do
    !$omp end parallel do

  end subroutine river_upscaler_celerity

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
