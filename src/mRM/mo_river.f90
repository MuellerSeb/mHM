!> \file    mo_river.f90
!> \copydoc mo_river

!> \brief   River representation.
!> \details This module contains a river network representation based on DAG and the grid class.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    May 2025
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! This code is released under the LGPLv3+ license \license_note
module mo_river

  use mo_kind,   only: i2, i4, i8, dp
  use mo_dag, only: dag, order_t, traversal_visit
  use mo_grid, only: grid_t, bottom_up, cartesian, dist_latlon
  use mo_grid_io, only: var, output_dataset
  use mo_message, only: error_message

  implicit none
  private

  !> \name D8 direction values
  !> \brief Constants describing the D8 routing direction of a cell.
  !!@{
  integer(i4), public, parameter :: sink = 0_i4 !< sink
  integer(i4), public, parameter :: dir_E = 1_i4 !< east
  integer(i4), public, parameter :: dir_SE = 2_i4 !< south-east
  integer(i4), public, parameter :: dir_S = 4_i4 !< south
  integer(i4), public, parameter :: dir_SW = 8_i4 !< south-west
  integer(i4), public, parameter :: dir_W = 16_i4 !< west
  integer(i4), public, parameter :: dir_NW = 32_i4 !< north-west
  integer(i4), public, parameter :: dir_N = 64_i4 !< north
  integer(i4), public, parameter :: dir_NE = 128_i4 !< north-east
  !> all directions as array
  integer(i4), public, dimension(8), parameter :: dirs = [dir_E, dir_SE, dir_S, dir_SW, dir_W, dir_NW, dir_N, dir_NE]
  !> matching back pointing directions as array
  integer(i4), public, dimension(8), parameter :: back = [dir_W, dir_NW, dir_N, dir_NE, dir_E, dir_SE, dir_S, dir_SW]
  !!@}

  !> \class river_t
  !> \brief River network representation
  type, extends(dag), public :: river_t
    integer(i8) :: n_nodes !< number of nodes in the river (D8-river: number of grid cells, SCC-river: number of all nodes)
    type(grid_t), pointer :: grid => null() !< grid the river network is defined on
    integer(i4), allocatable :: fdir(:) !< D8 flow direction (only for a D8-river) size(ncells)
    integer(i8), allocatable :: facc(:) !< flow accumulation size(n_nodes)
    integer(i8), allocatable :: down(:) !< downstream cell by id size(n_nodes)
    logical, allocatable :: is_sink(:) !< flag to indicate sinks size(n_nodes)
    integer(i8), allocatable :: sinks(:) !< node ids of sinks
    real(dp), allocatable :: link_length(:) !< length of link starting at node (0 if node is sink) size(n_nodes)
    real(dp), allocatable :: link_slope(:) !< slope of link starting at node (in [0,1]) size(n_nodes)
    real(dp), allocatable :: celerity(:) !< celerity of link starting at node derived form slope size(n_nodes)
    type(order_t) :: order !< level based order of the network
    ! scc related attributes
    logical :: scc = .false. !< indicate that this river is a SCC-river (not D8)
    integer(i8), allocatable :: node_cell(:) !< map node to grid cell id size(n_nodes)
    real(dp), allocatable :: area_fraction(:) !< area fraction for each node in cell size(n_nodes)
  contains
    procedure, public :: from_fdir => river_from_fdir
    procedure, public :: calc_order => river_order
    procedure, public :: calc_fdir => river_fdir
    procedure, public :: calc_facc => river_facc
    procedure, public :: calc_length => river_length
    procedure, public :: export => river_export
  end type river_t

contains

  !> \brief Get all links to and from given cell depending on flow direction
  pure subroutine get_links(mask, cells, fdir, i, j, dy, periodic, n_up, up, down)
    logical, dimension(:,:), intent(in) :: mask !< grid mask
    integer(i8), dimension(:,:), intent(in) :: cells !< cells id matrix
    integer(i4), dimension(:), intent(in) :: fdir !< flow direction array
    integer(i4), intent(in) :: i !< i index of current cell (on x-axis)
    integer(i4), intent(in) :: j !< j index of current cell (on y-axis)
    integer(i4), intent(in) :: dy !< direction of north in the grid matrix (1/-1)
    logical, intent(in) :: periodic !< flag to indicate periodic latlon grid (360 deg lon axis)
    integer(i4), intent(out) :: n_up !< number of upstream neighbor cells
    integer(i8), dimension(8), intent(out) :: up !< all upstream neighbor cells
    integer(i8), intent(out) :: down !< the downstream cell
    integer(i4) :: imax, jmax, ni, nj, k
    imax = size(cells, dim=1)
    jmax = size(cells, dim=2)
    ! upstream
    n_up = 0_i4
    do k = 1_i4, 8_i4
      call next(dirs(k), dy, i, j, ni, nj) ! check all directions
      if (periodic) then ! sinks still indicated by nj=0
        if (ni < 1_i4) ni = imax
        if (imax < ni) ni = 1_i4
      end if
      if (ni < 1_i4 .or. imax < ni .or. nj < 1_i4 .or. jmax < nj ) cycle ! outside matrix / sink
      if (.not.mask(ni,nj)) cycle ! outside mask
      if (fdir(cells(ni,nj)) /= back(k)) cycle ! check if this next cell is pointing back
      n_up = n_up + 1_i4 ! found an inflow
      up(n_up) = cells(ni,nj) ! add the upstream neighbor to the list
    end do
    ! downstream
    down = 0_i8
    call next(fdir(cells(i,j)), dy, i, j, ni, nj) ! get downstream cell
    if (periodic) then ! sinks still indicated by nj=0
      if (ni < 1_i4) ni = imax
      if (imax < ni) ni = 1_i4
    end if
    if (ni < 1_i4 .or. imax < ni .or. nj < 1_i4 .or. jmax < nj ) return ! outside matrix / sink
    if (.not.mask(ni,nj)) return ! outside mask
    down = cells(ni,nj)
  end subroutine get_links

  !> \brief Get next matrix indices from flow direction: (i,j) -> (ni,nj)
  pure subroutine next(fdir, dy, i, j, ni, nj)
    integer(i4), intent(in) :: fdir !< flow direction
    integer(i4), intent(in) :: dy !< direction of north in the grid matrix (1/-1)
    integer(i4), intent(in) :: i !< i index of current cell (on x-axis)
    integer(i4), intent(in) :: j !< j index of current cell (on y-axis)
    integer(i4), intent(out) :: ni !< i index of next cell (on x-axis)
    integer(i4), intent(out) :: nj !< j index of next cell (on y-axis)
    select case(fdir)
      case(dir_E) ! E
        ni = i+1_i4
        nj = j
      case(dir_SE) ! SE
        ni = i+1_i4
        nj = j-dy
      case(dir_S) ! S
        ni = i
        nj = j-dy
      case(dir_SW) ! SW
        ni = i-1_i4
        nj = j-dy
      case(dir_W) ! W
        ni = i-1_i4
        nj = j
      case(dir_NW) ! NW
        ni = i-1_i4
        nj = j+dy
      case(dir_N) ! N
        ni = i
        nj = j+dy
      case(dir_NE) ! NE
        ni = i+1_i4
        nj = j+dy
      case default ! sink/outlet
        ni = 0_i4
        nj = 0_i4
    end select
  end subroutine next

  !> \brief Get next matrix indices from flow direction: (i,j) -> (ni,nj)
  pure integer(i4) function get_fdir(from, to, dy)
    integer(i4), intent(in) :: from(2) !< from-cell indices
    integer(i4), intent(in) :: to(2) !< to-cell indices
    integer(i4), intent(in) :: dy !< direction of north in the grid matrix (1/-1)
    if (to(1)==(from(1)+1_i4) .and. to(2)==from(2)) then
      get_fdir = dir_E
    else if (to(1)==(from(1)+1_i4) .and. to(2)==(from(2)-dy)) then
      get_fdir = dir_SE
    else if (to(1)==from(1) .and. to(2)==(from(2)-dy)) then
      get_fdir = dir_S
    else if (to(1)==(from(1)-1_i4) .and. to(2)==(from(2)-dy)) then
      get_fdir = dir_SW
    else if (to(1)==(from(1)-1_i4) .and. to(2)==from(2)) then
      get_fdir = dir_W
    else if (to(1)==(from(1)-1_i4) .and. to(2)==(from(2)+dy)) then
      get_fdir = dir_NW
    else if (to(1)==from(1) .and. to(2)==(from(2)+dy)) then
      get_fdir = dir_N
    else if (to(1)==(from(1)+1_i4) .and. to(2)==(from(2)+dy)) then
      get_fdir = dir_NE
    else
      get_fdir = sink
    end if
  end function get_fdir

  !> \brief Initialize river network from flow direction.
  subroutine river_from_fdir(this, fdir, grid)
    class(river_t), intent(inout) :: this
    integer(i4), dimension(:) :: fdir !< D8 flow direction
    type(grid_t), pointer, intent(in), optional :: grid !< grid the river network is defined on
    integer(i8), allocatable :: cells(:,:)
    integer(i4) :: dy ! direction of north in the grid matrix (1/-1)
    integer(i4) :: n_up ! number of upstream neighbor cells
    integer(i8), dimension(8) :: up ! all upstream neighbor cells
    integer(i8) :: down ! the downstream cell
    integer(i8) :: i, j
    logical :: periodic ! periodic latlon grid
    if (present(grid)) this%grid => grid
    if (.not.associated(this%grid)) call error_message("river%from_fdir: grid not associated")
    call this%init(this%grid%ncells)
    this%fdir = fdir
    allocate(this%down(this%grid%ncells))
    this%n_nodes = this%grid%ncells
    this%scc = .false. ! if derived from fdir, this is a D8-river
    this%node_cell = [(i, i=1_i4,this%grid%ncells)] ! nodes correspond to cells
    allocate(this%area_fraction(this%n_nodes), source=1.0_dp) ! D8 has no area fraction

    periodic = this%grid%is_periodic()
    dy = -1_i4 ! top-down grid starts north
    if (this%grid%y_direction==bottom_up) dy = 1_i4

    cells = this%grid%id_matrix()
    !$omp parallel do default(shared) private(i, n_up, up, down)
    do i = 1_i8, this%grid%ncells
      call get_links(this%grid%mask, cells, this%fdir, this%grid%cell_ij(i,1), this%grid%cell_ij(i,2), dy, periodic, n_up, up, down)
      ! implicit (re-)allocation of LHS not working with intel-llvm + openmp
      allocate(this%nodes(i)%edges(n_up))
      this%nodes(i)%edges(:) = up(1_i4:n_up)
      if (down > 0_i8) then
        allocate(this%nodes(i)%dependents(1))
        this%nodes(i)%dependents(1) = down
      end if
      this%down(i) = down
    end do
    !$omp end parallel do
    deallocate(cells)

    ! determine sinks
    this%is_sink = (this%down == 0_i8)
    allocate(this%sinks(count(this%is_sink)))
    j = 0_i8
    do i = 1_i8, this%grid%ncells
      if (.not.this%is_sink(i)) cycle
      j = j + 1_i8
      this%sinks(j) = i
    end do

  end subroutine river_from_fdir

  !> \brief Get river node order by levels
  subroutine river_order(this)
    class(river_t), intent(inout) :: this
    integer(i8) :: istat
    if (allocated(this%order%id)) deallocate(this%order%id)
    if (allocated(this%order%level_start)) deallocate(this%order%level_start)
    if (allocated(this%order%level_end)) deallocate(this%order%level_end)
    call this%levelsort(this%order, istat)
    if (istat /= 0_i8) call error_message("river%order: found cycle")
  end subroutine river_order

  !> \brief Calculate flow accumulation
  subroutine river_facc(this)
    use mo_message, only: error_message
    class(river_t), intent(inout) :: this
    integer(i8) :: i, j, n, m
    if (allocated(this%facc)) deallocate(this%facc)
    allocate(this%facc(this%n_nodes))
    if (.not.allocated(this%order%id)) call error_message("river%calc_facc: order not initialized")
    do i = 1_i8, size(this%order%level_start, kind=i8)
      !$omp parallel do default(shared) private(j, n, m)
      do j = this%order%level_start(i), this%order%level_end(i)
        n = this%order%id(j)
        this%facc(n) = 1_i8
        do m = 1, this%nodes(n)%nedges()
          this%facc(n) = this%facc(n) + this%facc(this%nodes(n)%edges(m))
        end do
      end do
      !$omp end parallel do
    end do
  end subroutine river_facc

  !> \brief Calculate D8 flow direction from network
  subroutine river_fdir(this)
    use mo_message, only: error_message
    class(river_t), intent(inout) :: this
    integer(i4) :: from(2), to(2)
    integer(i8) :: i, j
    integer(i4) :: dy ! direction of north in the grid matrix (1/-1)
    logical :: periodic ! periodic latlon grid
    if (this%scc) call error_message("river%calc_fdir: can not calculated fdir for a SCC river.")
    if (allocated(this%fdir)) deallocate(this%fdir)
    allocate(this%fdir(this%n_nodes), source=0_i4)
    periodic = this%grid%is_periodic()
    dy = -1_i4 ! top-down grid starts north
    if (this%grid%y_direction==bottom_up) dy = 1_i4
    !$omp parallel do default(shared) private(i, j, from, to)
    do i = 1_i4, this%n_nodes
      j = this%down(i)
      if (j==0_i4) cycle ! sink
      from = this%grid%cell_ij(i,:)
      to = this%grid%cell_ij(j,:)
      ! the pathological case here is a periodic grid with 2 cells along lon-axis (*lol*)
      if (periodic .and. abs(from(1)-to(1))>1_i4) then
        ! set the node at the grid end to the begining
        if (from(1)==this%grid%nx) from(1) = 0_i4
        if (to(1)==this%grid%nx) to(1) = 0_i4
      end if
      this%fdir(i) = get_fdir(from, to, dy)
    end do
    !$omp end parallel do
  end subroutine river_fdir

  !> \brief Calculate link lengths
  subroutine river_length(this)
    use mo_constants, only: SQRT2_dp
    class(river_t), intent(inout) :: this
    integer(i8) :: i, to
    real(dp), allocatable, dimension(:) :: x_axis, y_axis
    if (this%scc) call error_message("river%calc_length: can't calculate D8 link lengths for a SCC river.")
    if (allocated(this%link_length)) deallocate(this%link_length)
    allocate(this%link_length(this%grid%ncells), source=0.0_dp)
    if (this%grid%coordsys == cartesian) then
      !$omp parallel do default(shared) private(i)
      do i = 1_i8, this%grid%ncells
        if (this%down(i) == 0_i8) cycle ! sinks ain't links
        select case (this%fdir(i))
          case(dir_E, dir_S, dir_W, dir_N)
            this%link_length(i) = this%grid%cellsize
          case(dir_SE, dir_SW, dir_NW, dir_NE)
            this%link_length(i) = SQRT2_dp * this%grid%cellsize
        end select
      end do
      !$omp end parallel do
    else
      x_axis = this%grid%x_axis()
      y_axis = this%grid%y_axis()
      !$omp parallel do default(shared) private(i, to)
      do i = 1_i8, this%grid%ncells
        to = this%down(i)
        if (to == 0_i8) cycle ! sinks ain't links
        this%link_length(i) = dist_latlon( &
          lat1=y_axis(this%grid%cell_ij(i, 2)), &
          lon1=x_axis(this%grid%cell_ij(i, 1)), &
          lat2=y_axis(this%grid%cell_ij(to, 2)), &
          lon2=x_axis(this%grid%cell_ij(to, 1)))
      end do
      !$omp end parallel do
    end if
  end subroutine river_length

  !> \brief Export river arrays to netcdf
  subroutine river_export(this, path, sub_map, leaving)
    class(river_t), intent(in) :: this
    character(*), intent(in) :: path !< path to the file
    integer(i4), intent(in), optional :: sub_map(this%n_nodes) !< map of sub-catchment IDs
    integer(i4), intent(in), optional :: leaving(this%n_nodes) !< map of sub-catchment IDs
    type(output_dataset) :: ds
    type(var), allocatable :: vars(:)
    integer(i8) :: i
    integer(i4) :: j
    integer(i4), allocatable :: level(:)
    if (this%scc) call error_message("river%export: can not export SCC river to gridded netcdf")
    allocate(vars(0))
    vars = [vars, var("fdir", "flow direction", dtype="i32", static=.true.)]
    if (allocated(this%facc)) vars = [vars, var("facc", "flow accumulation", dtype="i32", kind="i8", static=.true.)]
    if (allocated(this%order%id)) vars = [vars, var("level", "order level", dtype="i32", static=.true.)]
    if (allocated(this%link_length)) vars = [vars, var("length", "link length", dtype="f64", static=.true.)]
    if (present(sub_map)) vars = [vars, var("scc", "scc catchment id", dtype="i32", static=.true.)]
    if (present(leaving)) vars = [vars, var("leaving", "leaving", dtype="i32", static=.true.)]
    call ds%init(path, this%grid, vars)
    call ds%update("fdir", this%fdir)
    if (allocated(this%facc)) call ds%update("facc", this%facc)
    if (allocated(this%order%id)) then
      allocate(level(this%grid%ncells))
      do j = 1_i4, size(this%order%level_start)
        do i = this%order%level_start(j), this%order%level_end(j)
          level(this%order%id(i)) = j
        end do
      end do
      call ds%update("level", level)
    end if
    if (allocated(this%link_length)) call ds%update("length", this%link_length)
    if (present(sub_map)) call ds%update("scc", sub_map)
    if (present(leaving)) call ds%update("leaving", leaving)
    call ds%write()
    call ds%close()
    deallocate(vars)
  end subroutine river_export

end module mo_river
