!> \file    mo_mpr_container.f90
!> \copydoc mo_mpr_container

!> \brief   Module for a mpr process container.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Aug 2025
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_exchange
#include "logging.h"
module mo_mpr_container
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
  use mo_logging
  use mo_kind, only: i4, dp
  use mo_exchange_type, only: exchange_t
  use mo_grid, only: grid_t
  use mo_grid_scaler, only: scaler_t
  use mo_orderpack, only: sort_index
  use mo_string_utils, only: n2s => num2str
  use mo_utils, only: eq, is_close
  use mo_read_lut, only: read_geoformation_lut
  use nml_config_mpr, only: nml_config_mpr_t, NML_OK

  !> \class   mpr_t
  !> \brief   Class for a single MPR process container.
  type, public :: mpr_t
    type(nml_config_mpr_t) :: config !< configuration of the MPR process container
    type(exchange_t), pointer :: exchange => null() !< exchange container of the domain
    type(grid_t) :: tgt_level1 !< internal level1 grid derived from level0 when needed
    type(scaler_t) :: upscaler !< scaler from level0 morphology to level1 hydrology
    character(:), allocatable :: soil_lut_path !< resolved soil LUT path
    character(:), allocatable :: geo_lut_path !< resolved geology LUT path
    character(:), allocatable :: lai_path !< resolved gridded LAI path
    character(:), allocatable :: lai_lut_path !< resolved LAI LUT path
    real(dp), allocatable :: slope_emp(:) !< empirical slope distribution on level0
  contains
    procedure :: configure => mpr_configure
    procedure :: connect => mpr_connect
    procedure :: initialize => mpr_initialize
    procedure :: update => mpr_update
    procedure :: finalize => mpr_finalize
    procedure, private :: ensure_level1_grid => mpr_ensure_level1_grid
    procedure, private :: init_upscaler => mpr_init_upscaler
    procedure, private :: init_slope_emp => mpr_init_slope_emp
    procedure, private :: check_geo_units_against_lut => mpr_check_geo_units_against_lut
    procedure, private :: check_geoparameter_consistency => mpr_check_geoparameter_consistency
  end type mpr_t

contains

  !> \brief Configure the MPR process container.
  subroutine mpr_configure(self, file)
    class(mpr_t), intent(inout), target :: self
    character(*), intent(in), optional :: file !< file containing the namelists
    integer(i4) :: id(1)
    integer(i4) :: depth_idx(2)
    integer(i4) :: required_depths
    integer(i4) :: i
    character(1024) :: errmsg
    character(:), allocatable :: path
    integer :: status
    log_info(*) "Configure MPR"
    if (present(file)) then
      path = self%exchange%get_path(file) ! get absolute path relative to cwd
      log_info(*) "Read MPR config: ", path
      status = self%config%from_file(file=path, errmsg=errmsg)
      if (status /= NML_OK) then
        log_fatal(*) "Error reading MPR config: ", trim(errmsg)
        error stop 1
      end if
    end if
    if (.not.self%config%is_configured) then
      log_fatal(*) "MPR config not set."
      error stop 1
    end if
    status = self%config%is_valid(errmsg=errmsg)
    if (status /= NML_OK) then
      log_fatal(*) "MPR config not valid: ", trim(errmsg)
      error stop 1
    end if

    id(1) = self%exchange%domain

    status = self%config%is_set("n_horizons", idx=id, errmsg=errmsg)
    if (status /= NML_OK) then
      log_fatal(*) "MPR: n_horizons not set for domain ", n2s(id(1)), ". Error: ", trim(errmsg)
      error stop 1
    end if
    if (self%config%n_horizons(id(1)) < 1_i4) then
      log_fatal(*) "MPR: n_horizons must be >= 1 for domain ", n2s(id(1)), "."
      error stop 1
    end if

    status = self%config%is_set("lai_time_step", idx=id, errmsg=errmsg)
    if (status /= NML_OK) then
      log_fatal(*) "MPR: lai_time_step not set for domain ", n2s(id(1)), ". Error: ", trim(errmsg)
      error stop 1
    end if

    select case (self%config%soil_db_mode(id(1)))
      case (0_i4)
        required_depths = max(0_i4, self%config%n_horizons(id(1)) - 1_i4)
      case (1_i4)
        required_depths = self%config%n_horizons(id(1))
      case default
        log_fatal(*) "MPR: unsupported soil_db_mode=", n2s(self%config%soil_db_mode(id(1))), "."
        error stop 1
    end select
    do i = 1_i4, required_depths
      depth_idx = [i, id(1)]
      status = self%config%is_set("soil_depth", idx=depth_idx, errmsg=errmsg)
      if (status /= NML_OK) then
        log_fatal(*) "MPR: soil_depth(", n2s(i), ",", n2s(id(1)), ") not set. Error: ", trim(errmsg)
        error stop 1
      end if
    end do

    status = self%config%is_set("soil_lut_path", idx=id, errmsg=errmsg)
    if (status /= NML_OK) then
      log_fatal(*) "MPR: soil_lut_path not set for domain ", n2s(id(1)), ". Error: ", trim(errmsg)
      error stop 1
    end if
    self%soil_lut_path = self%exchange%get_path(self%config%soil_lut_path(id(1)))

    status = self%config%is_set("geo_lut_path", idx=id, errmsg=errmsg)
    if (status /= NML_OK) then
      log_fatal(*) "MPR: geo_lut_path not set for domain ", n2s(id(1)), ". Error: ", trim(errmsg)
      error stop 1
    end if
    self%geo_lut_path = self%exchange%get_path(self%config%geo_lut_path(id(1)))

    if (self%config%lai_time_step(id(1)) == 0_i4) then
      status = self%config%is_set("lai_lut_path", idx=id, errmsg=errmsg)
      if (status /= NML_OK) then
        log_fatal(*) "MPR: lai_lut_path not set for domain ", n2s(id(1)), &
          " while lai_time_step=0. Error: ", trim(errmsg)
        error stop 1
      end if
      self%lai_lut_path = self%exchange%get_path(self%config%lai_lut_path(id(1)))
      if (allocated(self%lai_path)) deallocate(self%lai_path)
    else
      status = self%config%is_set("lai_path", idx=id, errmsg=errmsg)
      if (status /= NML_OK) then
        log_fatal(*) "MPR: lai_path not set for domain ", n2s(id(1)), &
          " while lai_time_step=", n2s(self%config%lai_time_step(id(1))), ". Error: ", trim(errmsg)
        error stop 1
      end if
      self%lai_path = self%exchange%get_path(self%config%lai_path(id(1)))
      if (allocated(self%lai_lut_path)) deallocate(self%lai_lut_path)
    end if
  end subroutine mpr_configure

  !> \brief Connect the MPR process container with other components.
  subroutine mpr_connect(self)
    class(mpr_t), intent(inout), target :: self
    integer(i4) :: id(1)
    integer(i4) :: soil_layers
    log_info(*) "Connect MPR"

    id(1) = self%exchange%domain

    ! declare MPR prerequisites in the exchange contract
    self%exchange%slope%required = .true.
    self%exchange%aspect%required = .true.
    self%exchange%soil_id%required = .true.
    self%exchange%geo_unit%required = .true.
    self%exchange%gridded_lai%required = self%config%lai_time_step(id(1)) /= 0_i4

    if (.not.self%exchange%slope%provided) then
      log_fatal(*) "MPR: slope not provided (check input settings)."
      error stop 1
    end if
    if (.not.associated(self%exchange%slope%data)) then
      log_fatal(*) "MPR: slope marked as provided but data is not connected."
      error stop 1
    end if

    if (.not.self%exchange%aspect%provided) then
      log_fatal(*) "MPR: aspect not provided (check input settings)."
      error stop 1
    end if
    if (.not.associated(self%exchange%aspect%data)) then
      log_fatal(*) "MPR: aspect marked as provided but data is not connected."
      error stop 1
    end if

    if (.not.self%exchange%geo_unit%provided) then
      log_fatal(*) "MPR: geo_unit not provided (check geo_class input settings)."
      error stop 1
    end if
    if (.not.associated(self%exchange%geo_unit%data)) then
      log_fatal(*) "MPR: geo_unit marked as provided but data is not connected."
      error stop 1
    end if

    if (.not.self%exchange%soil_id%provided) then
      log_fatal(*) "MPR: soil_id not provided (check soil_class/soil_horizon_class input settings)."
      error stop 1
    end if
    if (.not.associated(self%exchange%soil_id%data)) then
      log_fatal(*) "MPR: soil_id marked as provided but data is not connected."
      error stop 1
    end if

    soil_layers = size(self%exchange%soil_id%data, 2)
    select case (self%config%soil_db_mode(id(1)))
      case (0_i4)
        if (soil_layers /= 1_i4) then
          log_fatal(*) "MPR: soil_db_mode=0 expects a single soil class layer, but got ", n2s(soil_layers), "."
          error stop 1
        end if
      case (1_i4)
        if (self%config%n_horizons(id(1)) < 1_i4) then
          log_fatal(*) "MPR: n_horizons must be >= 1 for soil_db_mode=1."
          error stop 1
        end if
        if (soil_layers < self%config%n_horizons(id(1))) then
          log_fatal(*) "MPR: soil horizon input provides ", n2s(soil_layers), " layers, but n_horizons=", &
            n2s(self%config%n_horizons(id(1))), "."
          error stop 1
        end if
      case default
        log_fatal(*) "MPR: unsupported soil_db_mode=", n2s(self%config%soil_db_mode(id(1))), "."
        error stop 1
    end select

    if (self%exchange%gridded_lai%required) then
      if (.not.self%exchange%gridded_lai%provided) then
        log_fatal(*) "MPR: gridded_lai not provided, but required for lai_time_step=", n2s(self%config%lai_time_step(id(1))), "."
        error stop 1
      end if
      if (.not.associated(self%exchange%gridded_lai%data)) then
        log_fatal(*) "MPR: gridded_lai marked as provided but data is not connected."
        error stop 1
      end if
      if (.not.allocated(self%lai_path)) then
        log_fatal(*) "MPR: internal error, lai_path not resolved in configure."
        error stop 1
      end if
    else if (.not.allocated(self%lai_lut_path)) then
      log_fatal(*) "MPR: internal error, lai_lut_path not resolved in configure."
      error stop 1
    end if

    if (.not.allocated(self%soil_lut_path)) then
      log_fatal(*) "MPR: internal error, soil_lut_path not resolved in configure."
      error stop 1
    end if
    if (.not.allocated(self%geo_lut_path)) then
      log_fatal(*) "MPR: internal error, geo_lut_path not resolved in configure."
      error stop 1
    end if
    if (.not.associated(self%exchange%level0)) then
      log_fatal(*) "MPR: level0 grid not connected."
      error stop 1
    end if
    call self%ensure_level1_grid()
    if (size(self%exchange%slope%data) /= self%exchange%level0%nCells) then
      log_fatal(*) "MPR: slope size (", n2s(size(self%exchange%slope%data)), &
        ") does not match level0 nCells (", n2s(int(self%exchange%level0%nCells, i4)), ")."
      error stop 1
    end if
    call self%init_slope_emp()

    if (.not.associated(self%exchange%geo_class_def)) allocate(self%exchange%geo_class_def)
    call self%exchange%geo_class_def%reset()
    log_info(*) "MPR: read geology LUT: ", self%geo_lut_path
    call read_geoformation_lut( &
      filename=trim(self%geo_lut_path), nGeo=self%exchange%geo_class_def%nGeo, &
      geo_unit=self%exchange%geo_class_def%geo_unit, geo_karstic=self%exchange%geo_class_def%geo_karstic)
    if (self%exchange%geo_class_def%nGeo < 1_i4) then
      log_fatal(*) "MPR: geology LUT contains no classes: ", self%geo_lut_path
      error stop 1
    end if
    call self%init_upscaler()
    call self%check_geo_units_against_lut()
    call self%check_geoparameter_consistency()
  end subroutine mpr_connect

  !> \brief Ensure level1 grid is available and consistent with configured level1 resolution.
  subroutine mpr_ensure_level1_grid(self)
    class(mpr_t), intent(inout), target :: self
    real(dp) :: l0_res
    real(dp) :: l1_res

    if (.not.associated(self%exchange%level0)) then
      log_fatal(*) "MPR: level0 grid not connected."
      error stop 1
    end if

    l1_res = self%exchange%level1_resolution
    if (.not.ieee_is_finite(l1_res) .or. l1_res <= 0.0_dp) then
      log_fatal(*) "MPR: level1 resolution not configured (expected from config_mhm/resolution)."
      error stop 1
    end if

    l0_res = self%exchange%level0%cellsize
    if (l1_res < l0_res .and. .not.is_close(l1_res, l0_res)) then
      log_fatal(*) "MPR: level1 resolution must be >= level0 cellsize."
      error stop 1
    end if

    if (associated(self%exchange%level1)) then
      if (.not.is_close(self%exchange%level1%cellsize, l1_res)) then
        log_fatal(*) "MPR: level1 grid cellsize (", n2s(self%exchange%level1%cellsize), &
          ") conflicts with configured level1_resolution (", n2s(l1_res), ")."
        error stop 1
      end if
      ! Validate geometric compatibility and that level0 mask fills level1 masked cells.
      call self%exchange%level1%check_is_filled_by(self%exchange%level0, check_mask=.true.)
      return
    end if

    call self%exchange%level0%gen_grid(self%tgt_level1, target_resolution=l1_res)
    self%exchange%level1 => self%tgt_level1
    log_info(*) "MPR: derive level1 grid from level0 with resolution ", n2s(l1_res)
  end subroutine mpr_ensure_level1_grid

  !> \brief Initialize cached L0->L1 scaler used by MPR upscaling routines.
  subroutine mpr_init_upscaler(self)
    class(mpr_t), intent(inout), target :: self

    if (.not.associated(self%upscaler%source_grid, self%exchange%level0) .or. &
      .not.associated(self%upscaler%target_grid, self%exchange%level1)) then
      call self%upscaler%init(source_grid=self%exchange%level0, target_grid=self%exchange%level1)
    end if
  end subroutine mpr_init_upscaler

  !> \brief Build empirical slope distribution F(slope) on level0 from input slope map.
  subroutine mpr_init_slope_emp(self)
    class(mpr_t), intent(inout), target :: self
    integer(i4), allocatable :: slope_sorted_index(:)
    integer(i4) :: i
    integer(i4) :: i_sort
    integer(i4) :: i_sortpost
    integer(i4) :: n_cells

    n_cells = size(self%exchange%slope%data)
    if (n_cells < 1_i4) then
      log_fatal(*) "MPR: cannot build slope_emp from empty slope input."
      error stop 1
    end if
    if (allocated(self%slope_emp)) deallocate(self%slope_emp)
    allocate(self%slope_emp(n_cells), slope_sorted_index(n_cells))

    slope_sorted_index = sort_index(self%exchange%slope%data)
    self%slope_emp(slope_sorted_index(n_cells)) = real(n_cells, dp) / real(n_cells + 1_i4, dp)

    do i = n_cells - 1_i4, 1_i4, -1_i4
      i_sort = slope_sorted_index(i)
      i_sortpost = slope_sorted_index(i + 1_i4)
      if (eq(self%exchange%slope%data(i_sort), self%exchange%slope%data(i_sortpost))) then
        self%slope_emp(i_sort) = self%slope_emp(i_sortpost)
      else
        self%slope_emp(i_sort) = real(i, dp) / real(n_cells + 1_i4, dp)
      end if
    end do

    self%exchange%slope_emp%provided = .true.
    self%exchange%slope_emp%data => self%slope_emp
    deallocate(slope_sorted_index)
  end subroutine mpr_init_slope_emp

  !> \brief Ensure all geological classes in the input map are represented in the LUT.
  subroutine mpr_check_geo_units_against_lut(self)
    class(mpr_t), intent(in), target :: self
    integer(i4) :: i
    integer(i4) :: geo_id

    do i = 1_i4, size(self%exchange%geo_unit%data)
      geo_id = self%exchange%geo_unit%data(i)
      if (.not.any(self%exchange%geo_class_def%geo_unit == geo_id)) then
        log_fatal(*) "MPR: geological unit ", n2s(geo_id), &
          " from geo_unit input is not present in geology LUT."
        error stop 1
      end if
    end do
  end subroutine mpr_check_geo_units_against_lut

  !> \brief Ensure geoparameter size matches loaded geology LUT when baseflow is active.
  subroutine mpr_check_geoparameter_consistency(self)
    class(mpr_t), intent(in), target :: self
    integer(i4) :: n_geo_param
    integer(i4) :: n_geo_lut

    if (self%exchange%parameters%process_matrix(9, 1) == 0_i4) return

    n_geo_param = self%exchange%parameters%nGeoUnits
    n_geo_lut = self%exchange%geo_class_def%nGeo
    if (n_geo_param /= n_geo_lut) then
      log_fatal(*) "MPR: geoparameter count (", n2s(n_geo_param), ") does not match geology LUT classes (", n2s(n_geo_lut), ")."
      error stop 1
    end if
  end subroutine mpr_check_geoparameter_consistency

  !> \brief Initialize the MPR process container for the simulation.
  subroutine mpr_initialize(self)
    class(mpr_t), intent(inout), target :: self
    log_info(*) "Initialize MPR for domain ", n2s(self%exchange%domain)
  end subroutine mpr_initialize

  !> \brief Update the MPR process container for the current time step.
  subroutine mpr_update(self)
    class(mpr_t), intent(inout), target :: self
    log_trace(*) "Update MPR at step ", n2s(self%exchange%step_count)
  end subroutine mpr_update

  !> \brief Finalize the MPR process container after the simulation.
  subroutine mpr_finalize(self)
    class(mpr_t), intent(inout), target :: self
    nullify(self%exchange%slope_emp%data)
    self%exchange%slope_emp%provided = .false.
    if (allocated(self%slope_emp)) deallocate(self%slope_emp)
    log_info(*) "Finalize MPR for domain ", n2s(self%exchange%domain)
  end subroutine mpr_finalize
end module mo_mpr_container
