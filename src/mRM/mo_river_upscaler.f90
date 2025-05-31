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
  use mo_river, only: river_t, dir_E, dir_S, dir_W, dir_N, dir_SE, dir_SW, dir_NW, dir_NE
  use mo_grid, only: grid_t, id_bounds, bottom_up
  use mo_grid_scaler, only: regridder, up_scaling

  implicit none
  private

  !> \class river_upscaler_t
  !> \brief River network upscaler
  !> \details upscale river network respecting scc.
  !! Nodes are ordered in reverse sub-catchment order: first the nodes of the base catchment are added,
  !! then the nodes of the sub-catchment flowing into the base catchment and so on.
  type, public :: river_upscaler_t
    type(river_t), pointer :: fine_river !< river definition at fine grid
    type(river_t) :: coarse_river !< river definition at coarse grid
    type(grid_t), pointer :: coarse_grid !< coarse grid for the upscaled river network
    type(regridder) :: upscaler !< upscaler from fine to coarse grid
    logical, allocatable :: leaving_cells(:) !< mask marking fine cells leaving a coarse cell size(fine\%ncells)
    logical, allocatable :: stream_mask(:) !< mask marking the upscaled stream at fine grid size(fine\%ncells)
    integer(i8) :: n_nodes !< number of nodes in the coarse river (respecting scc)
    integer(i8), allocatable :: floodplain(:) !< map marking floodplain of a coarse river node with their id size(fine\%ncells)
    real(dp), allocatable :: floodplain_area(:) !< floodplain area for a coarse river node size(n_nodes)
    integer(i8), allocatable :: link_from(:) !< starting cell at fine grid of coarse river node (0 for sink) size(n_nodes)
    integer(i8), allocatable :: link_to(:) !< end cell at fine grid of coarse river node (0 for sink) size(n_nodes)
    real(dp), allocatable :: scc_fraction(:) !< area fraction for each coarse river node in source cell (link_from) size(n_nodes)
    integer(i4), allocatable :: n_scc_nodes(:) !< number of scc nodes per coarse grid cell size(coarse\%ncells)
    integer(i8), allocatable :: node_cell(:) !< map node to coarse grid cell id size(n_nodes)
    integer(i4), allocatable :: node_sub(:) !< map node to sub catchment id size(n_nodes)
    integer(i8), allocatable :: sub_size(:) !< number of coarse river nodes in each sub catchment size(nsub)
  contains
    procedure, public :: init => river_upscaler_init
  end type river_upscaler_t

contains
  !> \brief Setup river upscaler from fine river and coarse target grid.
  subroutine river_upscaler_init(this, fine_river, coarse_grid, tol)
    implicit none
    class(river_upscaler_t), intent(inout) :: this
    type(river_t), pointer, intent(in) :: fine_river !< river definition at fine grid
    type(grid_t), pointer, intent(in) :: coarse_grid !< coarse grid for the upscaled river network
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparison (default: 1.e-7)
    integer(i8), allocatable :: cells(:,:)
    integer(i4), allocatable :: scc_map(:,:)
    integer(i8) :: i, k
    integer(i4) :: j, sub, ix, iy, y_Nb, y_Sb ! north and south bound depending on grid y-direction

    this%fine_river => fine_river
    this%coarse_grid => coarse_grid
    call this%upscaler%init(this%fine_river%grid, this%coarse_grid, tol=tol)
    if (this%upscaler%scaling_mode /= up_scaling) call error_message("river_upscaler: target grid needs to be coarser then input!")

    ! find all leaving cells (fine cells at coarse cell borders pointing out the coarse cell)
    !------------------------------------------------------------------
    !                             x_lb    x_ub
    !                        y_Nb  NW--N--NE       sides:   E, S, W, N
    !                              |       |       corners: SE,SW,NW,NE
    !                              W  (i)  E       y_ub and y_lb with inverse sky direction if grid is top-down
    !                              |       |
    !                        y_Sb  SW--S--SE
    !------------------------------------------------------------------
    cells = this%fine_river%grid%unpack([(i, i=1_i8, this%fine_river%grid%ncells)])
    scc_map = this%fine_river%grid%unpack(this%fine_river%sub_map)
    allocate(this%leaving_cells(this%fine_river%grid%ncells), source=.false.)
    if (this%fine_river%nsub > 1_i4) then
      allocate(this%n_scc_nodes(this%coarse_grid%ncells), source=0_i4)
    else
      allocate(this%n_scc_nodes(this%coarse_grid%ncells), source=1_i4)
    end if

    !$omp parallel do default(shared) private(i, y_Nb, y_Sb, ix, iy, j)
    do i = 1_i8, this%coarse_grid%ncells
      ! determine north and south bound depending on y-direction
      if (this%fine_river%grid%y_direction == bottom_up) then
        y_Nb = this%upscaler%y_ub(i)
        y_Sb = this%upscaler%y_lb(i)
      else
        y_Nb = this%upscaler%y_lb(i)
        y_Sb = this%upscaler%y_ub(i)
      end if
      ! searching on side E
      ix = this%upscaler%x_ub(i)
      do iy = this%upscaler%y_lb(i) + 1_i4, this%upscaler%y_ub(i) - 1_i4
        if (any(this%fine_river%fdir(cells(ix,iy))==[dir_NE, dir_E, dir_SE])) this%leaving_cells(cells(ix,iy)) = .true.
      end do
      ! searching on side S
      iy = y_Sb
      do ix = this%upscaler%x_lb(i) + 1_i4, this%upscaler%x_ub(i) - 1_i4
        if (any(this%fine_river%fdir(cells(ix,iy))==[dir_SE, dir_S, dir_SW])) this%leaving_cells(cells(ix,iy)) = .true.
      end do
      ! searching on side W
      ix = this%upscaler%x_lb(i)
      do iy = this%upscaler%y_lb(i) + 1_i4, this%upscaler%y_ub(i) - 1_i4
        if (any(this%fine_river%fdir(cells(ix,iy))==[dir_NW, dir_W, dir_SW])) this%leaving_cells(cells(ix,iy)) = .true.
      end do
      ! searching on side N
      iy = y_Nb
      do ix = this%upscaler%x_lb(i) + 1_i4, this%upscaler%x_ub(i) - 1_i4
        if (any(this%fine_river%fdir(cells(ix,iy))==[dir_NE, dir_N, dir_NW])) this%leaving_cells(cells(ix,iy)) = .true.
      end do
      ! south corners
      iy = y_Sb
      ! searching on corner SE
      ix = this%upscaler%x_ub(i)
      if (any(this%fine_river%fdir(cells(ix,iy))==[dir_NE, dir_E, dir_SE, dir_S, dir_SW])) this%leaving_cells(cells(ix,iy)) = .true.
      ! searching on corner SW
      ix = this%upscaler%x_lb(i)
      if (any(this%fine_river%fdir(cells(ix,iy))==[dir_NW, dir_W, dir_SW, dir_S, dir_SE])) this%leaving_cells(cells(ix,iy)) = .true.
      ! north corners
      iy = y_Nb
      ! searching on corner NE
      ix = this%upscaler%x_ub(i)
      if (any(this%fine_river%fdir(cells(ix,iy))==[dir_NW, dir_N, dir_NE, dir_E, dir_SE])) this%leaving_cells(cells(ix,iy)) = .true.
      ! searching on corner NW
      ix = this%upscaler%x_lb(i)
      if (any(this%fine_river%fdir(cells(ix,iy))==[dir_SW, dir_W, dir_NW, dir_N, dir_NE])) this%leaving_cells(cells(ix,iy)) = .true.

      ! count number of scc nodes
      if (this%fine_river%nsub > 1_i4) then
        do j = 1_i4, this%fine_river%nsub
          if (any(scc_map(this%upscaler%x_lb(i):this%upscaler%x_ub(i),this%upscaler%y_lb(i):this%upscaler%y_ub(i))==j)) then
            this%n_scc_nodes(i) = this%n_scc_nodes(i) + 1_i4
          end if
        end do
      end if
    end do
    !$omp end parallel do

    ! total number of nodes in coarse river
    this%n_nodes = sum(this%n_scc_nodes)
    ! determine attributes for scc
    allocate(this%node_cell(this%n_nodes))
    allocate(this%node_sub(this%n_nodes))
    allocate(this%sub_size(this%fine_river%nsub), source=0_i8)
    k = 0_i8
    ! loop over sub-catchments in reverse order to start with base catchment (target cells need to exist first)
    do j = this%fine_river%nsub, 1_i4, -1_i4
      sub = int(this%fine_river%scc_order(j), i4)
      do i = 1_i8, this%coarse_grid%ncells
        if (any(scc_map(this%upscaler%x_lb(i):this%upscaler%x_ub(i),this%upscaler%y_lb(i):this%upscaler%y_ub(i))==sub)) then
          k = k + 1_i8
          this%node_cell(k) = i
          this%node_sub(k) = sub
          this%sub_size(sub) = this%sub_size(sub) + 1_i8
        end if
      end do
    end do
    if (k /= this%n_nodes) call error_message("river_upscaler: couldn't determine coarse cell for all scc nodes!")

    ! calcuate fractions in parallel
    allocate(this%scc_fraction(this%n_nodes))
    !$omp parallel do default(shared) private(k, i)
    do k = 1_i8, this%n_nodes
      i = this%node_cell(k)
      this%scc_fraction(k) = sum( &
        this%upscaler%weights, &
        mask=(scc_map(this%upscaler%x_lb(i):this%upscaler%x_ub(i),this%upscaler%y_lb(i):this%upscaler%y_ub(i))==this%node_sub(k)))
    end do
    !$omp end parallel do

    call this%coarse_river%init(this%n_nodes)
    ! TODO: determine edges of coarse river
    ! TODO: start with base catchment (nsub) -> determine coarse outlets

    deallocate(cells, scc_map)
  end subroutine river_upscaler_init
end module mo_river_upscaler
