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
  use mo_grid, only: grid_t, bottom_up, cartesian
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
  ! all directions as array
  integer(i4), public, dimension(8), parameter :: dirs = [dir_E, dir_SE, dir_S, dir_SW, dir_W, dir_NW, dir_N, dir_NE]
  ! all back pointing directions as array
  integer(i4), public, dimension(8), parameter :: back = [dir_W, dir_NW, dir_N, dir_NE, dir_E, dir_SE, dir_S, dir_SW]
  !!@}

  !> \class river_t
  !> \brief River network representation
  type, extends(dag), public :: river_t
    type(grid_t), pointer :: grid !< grid the river network is defined on
    integer(i4), allocatable :: fdir(:) !< D8 flow direction
    integer(i8), allocatable :: facc(:) !< D8 flow accumulation
    integer(i8), allocatable :: down(:) !< downstream cell by id
    logical, allocatable :: is_sink(:) !< flag to indicate sinks
    real(dp), allocatable :: slope(:) !< slope of cell (in percent)
    real(dp), allocatable :: celerity(:) !< celerity of cell derived form slope
    real(dp), allocatable :: link_length(:) !< link length from this cell to the next (0 if outlet)
    integer(i8), allocatable :: outlets(:) !< cell ids of outlets
    integer(i4) :: nsub = 1_i4 !< number of sub catchments
    integer(i4), allocatable :: sub_map(:) !< sub catchment id (id 'nsub' indicates base-catchment)
    integer(i8), allocatable :: sub_gauge(:) !< id of gauge for sub catchment size(nsub-1)
    type(dag) :: scc_tree !< dependency tree of sub-catchments
    integer(i8), allocatable :: scc_order(:) !< calculation order of sub catchments
    integer(i4) :: dy !< delta-y based on y-axis direction to correctly interpret fdir (top-down: 1, bottom-up: -1)
    type(order_t) :: order !< level based order of the network
  contains
    procedure, public :: from_fdir => river_from_fdir
    procedure, public :: calc_order => river_order
    procedure, public :: calc_facc => river_facc
    procedure, public :: calc_scc => river_scc
    procedure, public :: calc_link_length => river_length
    procedure, public :: export => river_export
  end type river_t

contains

  !> \brief Get all links to and from given cell depending on flow direction
  pure subroutine get_links(mask, cells, fdir, i, j, dy, n_up, up, down)
    logical, dimension(:,:), intent(in) :: mask !< grid mask
    integer(i8), dimension(:,:), intent(in) :: cells !< cells id matrix
    integer(i4), dimension(:), intent(in) :: fdir !< flow direction array
    integer(i4), intent(in) :: i !< i index of current cell (on x-axis)
    integer(i4), intent(in) :: j !< j index of current cell (on y-axis)
    integer(i4), intent(in) :: dy !< direction of south in the grid matrix (1/-1)
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
      if (ni < 1_i4 .or. imax < ni .or. nj < 1_i4 .or. jmax < nj ) cycle ! outside matrix / sink
      if (.not.mask(ni,nj)) cycle ! outside mask
      if (fdir(cells(ni,nj)) /= back(k)) cycle ! check if this next cell is pointing back
      n_up = n_up + 1_i4 ! found an inflow
      up(n_up) = cells(ni,nj) ! add the upstream neighbor to the list
    end do
    ! downstream
    down = 0_i8
    call next(fdir(cells(i,j)), dy, i, j, ni, nj) ! get downstream cell
    if (ni < 1_i4 .or. imax < ni .or. nj < 1_i4 .or. jmax < nj ) return ! outside matrix / sink
    if (.not.mask(ni,nj)) return ! outside mask
    down = cells(ni,nj)
  end subroutine get_links

  !> \brief Get next matrix indices from flow direction: (i,j) -> (ni,nj)
  pure subroutine next(fdir, dy, i, j, ni, nj)
    integer(i4), intent(in) :: fdir !< flow direction
    integer(i4), intent(in) :: dy !< direction of south in the grid matrix (1/-1)
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
        nj = j+dy
      case(dir_S) ! S
        ni = i
        nj = j+dy
      case(dir_SW) ! SW
        ni = i-1_i4
        nj = j+dy
      case(dir_W) ! W
        ni = i-1_i4
        nj = j
      case(dir_NW) ! NW
        ni = i-1_i4
        nj = j-dy
      case(dir_N) ! N
        ni = i
        nj = j-dy
      case(dir_NE) ! NE
        ni = i+1_i4
        nj = j-dy
      case default ! sink/outlet
        ni = 0_i4
        nj = 0_i4
    end select
  end subroutine next

  !> \brief Initialize river network from flow direction.
  subroutine river_from_fdir(this, grid, fdir)
    class(river_t), intent(inout) :: this
    type(grid_t), pointer, intent(in) :: grid !< grid the river network is defined on
    integer(i4), dimension(:) :: fdir !< D8 flow direction
    integer(i8), allocatable :: cells(:,:)
    integer(i4) :: n_up ! number of upstream neighbor cells
    integer(i8), dimension(8) :: up ! all upstream neighbor cells
    integer(i8) :: down ! the downstream cell
    integer(i8) :: i, j
    call this%init(grid%ncells)
    this%grid => grid
    this%fdir = fdir
    allocate(this%down(this%grid%ncells))

    this%dy = 1_i4
    if (this%grid%y_direction==bottom_up) this%dy = -1_i4

    cells = this%grid%unpack([(i, i=1_i8, this%grid%ncells)])
    !$omp parallel do default(shared) private(i, n_up, up, down)
    do i = 1_i8, this%grid%ncells
      call get_links(this%grid%mask, cells, this%fdir, this%grid%cell_ij(i,1), this%grid%cell_ij(i,2), this%dy, n_up, up, down)
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

    ! determine outlets
    this%is_sink = (this%down == 0_i8)
    allocate(this%outlets(count(this%is_sink)))
    j = 0_i8
    do i = 1_i8, this%grid%ncells
      if (.not.this%is_sink(i)) cycle
      j = j + 1_i8
      this%outlets(j) = i
    end do

    ! scc related attributes for a single base catchment only
    allocate(this%sub_map(this%grid%ncells), source=1_i4)
    allocate(this%sub_gauge(0))
    call this%scc_tree%init(1_i8)
    this%scc_order = [1_i8]

  end subroutine river_from_fdir

  !> \brief Get river node order by levels
  subroutine river_order(this)
    class(river_t), intent(inout) :: this
    integer(i8) :: istat
    call this%levelsort(this%order, istat)
    if (istat /= 0_i8) call error_message("river%order: found cycle")
  end subroutine river_order

  !> \brief Calculate flow accumulation
  subroutine river_facc(this)
    use mo_message, only: error_message
    class(river_t), intent(inout) :: this
    integer(i8) :: i, j, n, m
    if (.not.allocated(this%facc)) allocate(this%facc(this%grid%ncells))
    if (.not.allocated(this%order%id)) call error_message("river: order not initialized")
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

  !> \brief Generate SCC map
  subroutine river_scc(this, gauges)
    use mo_message, only: error_message
    class(river_t), intent(inout) :: this
    integer(i4), dimension(:,:), intent(in) :: gauges !< gauge locations dim 1: x/y, dim 2: gauge id
    integer(i8), dimension(size(gauges, dim=2)) :: gauge_cell
    logical, dimension(this%grid%ncells) :: cell_loc
    logical, dimension(size(gauges, dim=2)) :: gauge_mask
    integer(i8), dimension(size(gauges, dim=2)) :: gauge_facc
    type(traversal_visit) :: handler
    integer(i4) :: i, j, n
    integer(i8) :: istat
    n = size(gauges, dim=2)
    this%nsub = n + 1_i4
    if (.not.allocated(this%facc)) call error_message("river: facc not initialized")
    ! find gauge cell ids
    do i = 1_i4, n
      cell_loc = (this%grid%cell_ij(:, 1) == gauges(1, i)).and.(this%grid%cell_ij(:, 2) == gauges(2, i))
      gauge_cell(i) = findloc(cell_loc, .true., dim=1, kind=i8)
      if (gauge_cell(i) == 0_i8) call error_message("river: gauge location not found")
      gauge_facc(i) = this%facc(gauge_cell(i))
    end do

    this%sub_map = n+1_i4 ! base catchment gets id n+1
    deallocate(this%sub_gauge)
    allocate(this%sub_gauge(n))
    allocate(handler%visited(this%grid%ncells))
    ! sort gauges by facc from highest to lowest
    gauge_mask = .true.
    do i = 1_i4, n
      ! sort sub-gauges by facc (high to low)
      j = maxloc(gauge_facc, mask=gauge_mask, dim=1)
      this%sub_gauge(i) = gauge_cell(i)
      gauge_mask(j) = .false.
      handler%visited = .false. ! reset traverse handler
      call this%traverse(handler, [gauge_cell(j)])
      where (handler%visited) this%sub_map = j
    end do
    deallocate(handler%visited)

    ! generate scc dependency tree
    call this%scc_tree%init(int(n+1_i4, kind=i8))
    do i = 1_i4, n
      if (this%is_sink(this%sub_gauge(i))) cycle ! sub gauge is an outlet
      call this%scc_tree%add_edge(int(this%sub_map(this%down(this%sub_gauge(i))), i8), int(i, i8))
    end do
    ! determine sub-catchment calculation order
    if (allocated(this%scc_order)) deallocate(this%scc_order)
    call this%scc_tree%toposort(this%scc_order, istat)

  end subroutine river_scc

  !> \brief Calculate link lengths
  subroutine river_length(this)
    use mo_constants, only: SQRT2_dp
    class(river_t), intent(inout) :: this
    integer(i8) :: i, to
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
      !$omp parallel do default(shared) private(i, to)
      do i = 1_i8, this%grid%ncells
        to = this%down(i)
        if (to == 0_i8) cycle ! sinks ain't links
        this%link_length(i) = dist_latlon( &
          lat1=this%grid%lat(this%grid%cell_ij(i, 1), this%grid%cell_ij(i, 2)), &
          lon1=this%grid%lon(this%grid%cell_ij(i, 1), this%grid%cell_ij(i, 2)), &
          lat2=this%grid%lat(this%grid%cell_ij(to, 1), this%grid%cell_ij(to, 2)), &
          lon2=this%grid%lon(this%grid%cell_ij(to, 1), this%grid%cell_ij(to, 2)))
      end do
      !$omp end parallel do
    end if
  end subroutine river_length

  !> \brief Export river arrays to netcdf
  subroutine river_export(this, path)
    class(river_t), intent(in) :: this
    character(*), intent(in) :: path !< path to the file
    type(output_dataset) :: ds
    type(var), allocatable :: vars(:)
    integer(i8) :: i
    integer(i4) :: j
    integer(i4), allocatable :: level(:)
    allocate(vars(0))
    vars = [vars, var("fdir", "flow direction", dtype="i32", static=.true.)]
    vars = [vars, var("id", "cell id", dtype="i32", kind="i8", static=.true.)]
    if (allocated(this%facc)) vars = [vars, var("facc", "flow accumulation", dtype="i32", kind="i8", static=.true.)]
    if (allocated(this%sub_map)) vars = [vars, var("scc", "scc catchment id", dtype="i32", static=.true.)]
    if (allocated(this%order%id)) vars = [vars, var("level", "order level", dtype="i32", static=.true.)]
    if (allocated(this%link_length)) vars = [vars, var("length", "link length", dtype="f64", static=.true.)]
    call ds%init(path, this%grid, vars)
    call ds%update("fdir", this%fdir)
    call ds%update("id", [(i, i=1_i8, this%grid%ncells)])
    if (allocated(this%facc)) call ds%update("facc", this%facc)
    if (allocated(this%sub_map)) call ds%update("scc", this%sub_map)
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
    call ds%write()
    call ds%close()
    deallocate(vars)
  end subroutine river_export

  !> \brief distance between two points on the sphere [m]
  pure real(dp) function dist_latlon(lat1, lon1, lat2, lon2)
    use mo_constants, only : RadiusEarth_dp, deg2rad_dp
    real(dp), intent(in) :: lat1
    real(dp), intent(in) :: lon1
    real(dp), intent(in) :: lat2
    real(dp), intent(in) :: lon2
    real(dp) :: theta1, phi1, theta2, phi2
    real(dp) :: term1, term2, term3, temp

    theta1 = deg2rad_dp * lon1
    phi1 = deg2rad_dp * lat1
    theta2 = deg2rad_dp * lon2
    phi2 = deg2rad_dp * lat2

    term1 = cos(phi1) * cos(theta1) * cos(phi2) * cos(theta2)
    term2 = cos(phi1) * sin(theta1) * cos(phi2) * sin(theta2)
    term3 = sin(phi1) * sin(phi2)
    temp = min(term1 + term2 + term3, 1.0_dp)
    dist_latlon = RadiusEarth_dp * acos(temp);
  end function dist_latlon

end module mo_river
