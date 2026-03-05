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
  use mo_constants, only: YearMonths
  use mo_kind, only: i4, dp
  use mo_exchange_type, only: exchange_t
  use mo_grid, only: grid_t
  use mo_grid_scaler, only: scaler_t
  use mo_orderpack, only: sort_index
  use mo_string_utils, only: n2s => num2str
  use mo_utils, only: eq, is_close, optval
  use mo_read_lut, only: read_geoformation_lut, read_lai_lut
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
    character(:), allocatable :: land_cover_path !< resolved land-cover path
    character(:), allocatable :: lai_path !< resolved gridded LAI path
    character(:), allocatable :: lai_lut_path !< resolved LAI LUT path
    real(dp), allocatable :: slope_emp(:) !< empirical slope distribution on level0
    real(dp), allocatable :: lai_l0_cache(:, :) !< cached LAI on level0 (nCells0, nLAI)
    real(dp), allocatable :: max_interception_cache(:, :, :) !< cached max interception (nCells1, nLAI, nLC)
    integer(i4) :: n_lai_periods = 1_i4 !< number of cached LAI periods
    integer(i4) :: n_land_cover_periods = 1_i4 !< number of cached land-cover periods
    integer(i4) :: active_lai_idx = 0_i4 !< active cached LAI index
    integer(i4) :: active_land_cover_idx = 0_i4 !< active cached land-cover index
  contains
    procedure :: configure => mpr_configure
    procedure :: connect => mpr_connect
    procedure :: initialize => mpr_initialize
    procedure :: update => mpr_update
    procedure :: finalize => mpr_finalize
    procedure, private :: ensure_level1_grid => mpr_ensure_level1_grid
    procedure, private :: init_upscaler => mpr_init_upscaler
    procedure, private :: init_slope_emp => mpr_init_slope_emp
    procedure, private :: init_temporal_cache => mpr_init_temporal_cache
    procedure, private :: build_lai_l0_cache => mpr_build_lai_l0_cache
    procedure, private :: init_max_interception_cache => mpr_init_max_interception_cache
    procedure, private :: update_exchange_slices => mpr_update_exchange_slices
    procedure, private :: lai_index_for_time => mpr_lai_index_for_time
    procedure, private :: land_cover_index_for_time => mpr_land_cover_index_for_time
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

    status = self%config%is_set("land_cover_path", idx=id, errmsg=errmsg)
    if (status /= NML_OK) then
      log_fatal(*) "MPR: land_cover_path not set for domain ", n2s(id(1)), ". Error: ", trim(errmsg)
      error stop 1
    end if
    self%land_cover_path = self%exchange%get_path(self%config%land_cover_path(id(1)))

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
    logical :: require_gridded_lai
    log_info(*) "Connect MPR"

    id(1) = self%exchange%domain

    ! declare MPR prerequisites in the exchange contract
    self%exchange%slope%required = .true.
    self%exchange%aspect%required = .true.
    self%exchange%soil_id%required = .true.
    self%exchange%geo_unit%required = .true.
    require_gridded_lai = self%config%lai_time_step(id(1)) /= 0_i4 .or. &
      self%exchange%parameters%process_matrix(1, 1) /= 0_i4
    self%exchange%gridded_lai%required = require_gridded_lai

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

    if (require_gridded_lai) then
      if (.not.self%exchange%gridded_lai%provided) then
        log_fatal(*) "MPR: gridded_lai not provided, but required for lai_time_step=", n2s(self%config%lai_time_step(id(1))), "."
        error stop 1
      end if
      if (.not.associated(self%exchange%gridded_lai%data)) then
        log_fatal(*) "MPR: gridded_lai marked as provided but data is not connected."
        error stop 1
      end if
    end if

    if (self%config%lai_time_step(id(1)) == 0_i4) then
      if (.not.allocated(self%lai_lut_path)) then
        log_fatal(*) "MPR: internal error, lai_lut_path not resolved in configure."
        error stop 1
      end if
    else
      if (.not.allocated(self%lai_path)) then
        log_fatal(*) "MPR: internal error, lai_path not resolved in configure."
        error stop 1
      end if
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
    call self%init_temporal_cache()
    call self%update_exchange_slices(force=.true.)
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

  !> \brief Build MPR temporal caches in connect for later pointer-only switching.
  subroutine mpr_init_temporal_cache(self)
    class(mpr_t), intent(inout), target :: self

    self%n_lai_periods = 1_i4
    self%n_land_cover_periods = 1_i4
    self%active_lai_idx = 0_i4
    self%active_land_cover_idx = 0_i4

    ! Start with interception cache as first time-dependent MPR output.
    if (self%exchange%parameters%process_matrix(1, 1) == 0_i4) return

    if (.not.associated(self%exchange%gridded_lai%data)) then
      log_fatal(*) "MPR: gridded_lai data must be connected before temporal cache initialization."
      error stop 1
    end if

    call self%build_lai_l0_cache()
    call self%init_max_interception_cache()
    log_info(*) "MPR: temporal cache initialized (nLAI=", n2s(self%n_lai_periods), &
      ", nLC=", n2s(self%n_land_cover_periods), ")."
  end subroutine mpr_init_temporal_cache

  !> \brief Build cached LAI fields on level0 for all currently supported LAI periods.
  subroutine mpr_build_lai_l0_cache(self)
    class(mpr_t), intent(inout), target :: self
    integer(i4) :: id(1)
    integer(i4) :: i_cell
    integer(i4) :: i_class
    integer(i4) :: lai_idx
    integer(i4) :: class_id
    integer(i4) :: class_pos
    integer(i4) :: n_lai_classes
    real(dp) :: class_value
    integer(i4), allocatable :: lai_id_list(:)
    real(dp), allocatable :: lai_lut(:, :)

    id(1) = self%exchange%domain

    if (allocated(self%lai_l0_cache)) deallocate(self%lai_l0_cache)

    select case (self%config%lai_time_step(id(1)))
      case (0_i4)
        if (.not.allocated(self%lai_lut_path)) then
          log_fatal(*) "MPR: internal error, lai_lut_path not resolved in configure."
          error stop 1
        end if
        call read_lai_lut(filename=trim(self%lai_lut_path), nLAI=n_lai_classes, LAIIDlist=lai_id_list, LAI=lai_lut)
        if (size(lai_lut, 2) < int(YearMonths, i4)) then
          log_fatal(*) "MPR: LAI LUT provides ", n2s(size(lai_lut, 2)), " periods, expected at least ", n2s(int(YearMonths, i4)), "."
          error stop 1
        end if

        self%n_lai_periods = int(YearMonths, i4)
        allocate(self%lai_l0_cache(size(self%exchange%gridded_lai%data), self%n_lai_periods))
        do i_cell = 1_i4, size(self%exchange%gridded_lai%data)
          class_value = self%exchange%gridded_lai%data(i_cell)
          class_id = nint(class_value)
          if (.not.is_close(class_value, real(class_id, dp))) then
            log_fatal(*) "MPR: LAI class map contains non-integer ID at cell ", n2s(i_cell), ": ", n2s(class_value), "."
            error stop 1
          end if

          class_pos = 0_i4
          do i_class = 1_i4, n_lai_classes
            if (lai_id_list(i_class) == class_id) then
              class_pos = i_class
              exit
            end if
          end do
          if (class_pos < 1_i4) then
            log_fatal(*) "MPR: LAI class ID ", n2s(class_id), " missing in LAI LUT: ", self%lai_lut_path
            error stop 1
          end if

          do lai_idx = 1_i4, self%n_lai_periods
            self%lai_l0_cache(i_cell, lai_idx) = min(30.0_dp, max(1.0e-10_dp, lai_lut(class_pos, lai_idx)))
          end do
        end do

      case default
        ! Current exchange contract carries one active gridded_lai slice.
        self%n_lai_periods = 1_i4
        allocate(self%lai_l0_cache(size(self%exchange%gridded_lai%data), 1))
        self%lai_l0_cache(:, 1) = min(30.0_dp, max(1.0e-10_dp, self%exchange%gridded_lai%data))
    end select
  end subroutine mpr_build_lai_l0_cache

  !> \brief Cache max interception on level1 for all LAI/land-cover slices.
  subroutine mpr_init_max_interception_cache(self)
    class(mpr_t), intent(inout), target :: self
    integer(i4) :: lai_idx
    real(dp), allocatable :: interception_param(:)
    real(dp), allocatable :: max_interception_l0(:)

    interception_param = self%exchange%parameters%get_process(1_i4)
    if (size(interception_param) < 1_i4) then
      log_fatal(*) "MPR: interception parameter set is empty while process 1 is active."
      error stop 1
    end if
    if (.not.allocated(self%lai_l0_cache)) then
      log_fatal(*) "MPR: LAI level0 cache not available before max interception cache initialization."
      error stop 1
    end if

    if (allocated(self%max_interception_cache)) deallocate(self%max_interception_cache)
    allocate(self%max_interception_cache(self%exchange%level1%ncells, self%n_lai_periods, self%n_land_cover_periods))
    allocate(max_interception_l0(size(self%lai_l0_cache, 1)))
    do lai_idx = 1_i4, self%n_lai_periods
      max_interception_l0 = interception_param(1) * self%lai_l0_cache(:, lai_idx)
      call self%upscaler%execute(max_interception_l0, self%max_interception_cache(:, lai_idx, 1))
    end do
    self%exchange%max_interception%provided = .true.
  end subroutine mpr_init_max_interception_cache

  !> \brief Switch exchange pointers to active cached MPR slices for current model time.
  subroutine mpr_update_exchange_slices(self, force)
    class(mpr_t), intent(inout), target :: self
    logical, optional, intent(in) :: force
    logical :: force_
    integer(i4) :: lai_idx
    integer(i4) :: land_cover_idx

    force_ = optval(force, .false.)

    if (.not.allocated(self%max_interception_cache)) return

    lai_idx = self%lai_index_for_time()
    land_cover_idx = self%land_cover_index_for_time()
    if (lai_idx < 1_i4 .or. lai_idx > self%n_lai_periods) then
      log_fatal(*) "MPR: LAI index out of bounds: ", n2s(lai_idx), " not in [1,", n2s(self%n_lai_periods), "]."
      error stop 1
    end if
    if (land_cover_idx < 1_i4 .or. land_cover_idx > self%n_land_cover_periods) then
      log_fatal(*) "MPR: land-cover index out of bounds: ", n2s(land_cover_idx), " not in [1,", n2s(self%n_land_cover_periods), "]."
      error stop 1
    end if
    if (.not.force_ .and. lai_idx == self%active_lai_idx .and. land_cover_idx == self%active_land_cover_idx) return

    self%exchange%max_interception%data => self%max_interception_cache(:, lai_idx, land_cover_idx)
    self%exchange%max_interception%provided = .true.
    self%active_lai_idx = lai_idx
    self%active_land_cover_idx = land_cover_idx
    log_trace(*) "MPR: active cache slice lai=", n2s(lai_idx), ", land_cover=", n2s(land_cover_idx)
  end subroutine mpr_update_exchange_slices

  !> \brief Resolve active LAI period index from current exchange time.
  integer(i4) function mpr_lai_index_for_time(self) result(lai_idx)
    class(mpr_t), intent(inout), target :: self
    integer(i4) :: id(1)

    id(1) = self%exchange%domain
    if (self%n_lai_periods < 1_i4) then
      log_fatal(*) "MPR: n_lai_periods must be >= 1."
      error stop 1
    end if

    lai_idx = 1_i4
    select case (self%config%lai_time_step(id(1)))
      case (0_i4, 1_i4)
        if (self%n_lai_periods >= int(YearMonths, i4)) lai_idx = self%exchange%time%month
      case default
        lai_idx = 1_i4
    end select
    lai_idx = max(1_i4, min(lai_idx, self%n_lai_periods))
  end function mpr_lai_index_for_time

  !> \brief Resolve active land-cover period index from current exchange time.
  integer(i4) function mpr_land_cover_index_for_time(self) result(land_cover_idx)
    class(mpr_t), intent(inout), target :: self

    if (self%n_land_cover_periods < 1_i4) then
      log_fatal(*) "MPR: n_land_cover_periods must be >= 1."
      error stop 1
    end if

    ! Land-cover period parsing is not integrated yet; use one cached period.
    land_cover_idx = 1_i4
  end function mpr_land_cover_index_for_time

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
    call self%update_exchange_slices(force=.true.)
  end subroutine mpr_initialize

  !> \brief Update the MPR process container for the current time step.
  subroutine mpr_update(self)
    class(mpr_t), intent(inout), target :: self
    log_trace(*) "Update MPR at step ", n2s(self%exchange%step_count)
    call self%update_exchange_slices()
  end subroutine mpr_update

  !> \brief Finalize the MPR process container after the simulation.
  subroutine mpr_finalize(self)
    class(mpr_t), intent(inout), target :: self
    nullify(self%exchange%slope_emp%data)
    self%exchange%slope_emp%provided = .false.
    if (allocated(self%slope_emp)) deallocate(self%slope_emp)
    nullify(self%exchange%max_interception%data)
    self%exchange%max_interception%provided = .false.
    if (allocated(self%lai_l0_cache)) deallocate(self%lai_l0_cache)
    if (allocated(self%max_interception_cache)) deallocate(self%max_interception_cache)
    self%n_lai_periods = 1_i4
    self%n_land_cover_periods = 1_i4
    self%active_lai_idx = 0_i4
    self%active_land_cover_idx = 0_i4
    log_info(*) "Finalize MPR for domain ", n2s(self%exchange%domain)
  end subroutine mpr_finalize
end module mo_mpr_container
