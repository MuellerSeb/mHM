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
  use mo_constants, only: nodata_dp
  use mo_datetime, only: datetime, YEAR_MONTHS, one_hour
  use mo_kind, only: i4, dp
  use mo_common_constants, only: soilHorizonsVarName, landCoverPeriodsVarName, LAIVarName
  use mo_exchange_type, only: exchange_t
  use mo_grid, only: grid_t, cartesian, spherical
  use mo_grid_io, only: input_dataset, start_timestamp, var
  use mo_grid_scaler, only: scaler_t, up_a_mean
  use mo_mpr_legacy_bridge, only: mpr_bridge_land_cover_fraction, mpr_bridge_snow_param, mpr_bridge_pet_lai, &
    mpr_bridge_pet_aspect, mpr_bridge_pet_hargreaves, mpr_bridge_pet_priestley_taylor, &
    mpr_bridge_pet_penman_monteith, &
    mpr_bridge_setup_soil_database, mpr_bridge_soil_moisture, mpr_bridge_runoff_param, mpr_bridge_karstic_param, &
    mpr_bridge_baseflow_param, mpr_bridge_sealed_threshold
  use mo_netcdf, only: NcDataset, NcDimension, NcVariable
  use mo_orderpack, only: sort_index
  use mo_string_utils, only: n2s => num2str
  use mo_utils, only: eq, is_close, optval
  use mo_read_lut, only: read_geoformation_lut, read_lai_lut
  use nml_config_mpr, only: nml_config_mpr_t, NML_OK

  character(len=*), parameter :: lc_restart_calendar = "proleptic_gregorian"

  type :: mpr_land_cover_state_t
    character(:), allocatable :: path !< resolved land-cover dataset path
    character(:), allocatable :: var_name !< configured land-cover variable name
    type(input_dataset) :: ds !< land-cover dataset input handler
    logical :: temporal = .false. !< whether land cover has temporal periods in file
    integer(i4), allocatable :: l0_cache(:, :) !< cached land cover on level0 (nCells0, nLC)
    real(dp), allocatable :: forest_fraction_l1(:, :) !< cached forest fraction at level1 (nCells1, nLC)
    real(dp), allocatable :: sealed_fraction_l1(:, :) !< cached sealed fraction at level1 (nCells1, nLC)
    real(dp), allocatable :: pervious_fraction_l1(:, :) !< cached pervious fraction at level1 (nCells1, nLC)
    type(datetime), allocatable :: period_start(:) !< cached land-cover period start times
    type(datetime), allocatable :: period_end(:) !< cached land-cover period end times
    integer(i4) :: n_periods = 1_i4 !< number of cached land-cover periods
    integer(i4) :: active_idx = 0_i4 !< active cached land-cover index
  end type mpr_land_cover_state_t

  type :: mpr_snow_state_t
    real(dp), allocatable :: thresh_temp_cache(:, :) !< cached temperature threshold (nCells1, nLC)
    real(dp), allocatable :: degday_dry_cache(:, :) !< cached dry degree-day factor (nCells1, nLC)
    real(dp), allocatable :: degday_inc_cache(:, :) !< cached degree-day precipitation increment (nCells1, nLC)
    real(dp), allocatable :: degday_max_cache(:, :) !< cached maximum degree-day factor (nCells1, nLC)
  end type mpr_snow_state_t

  type :: mpr_pet_state_t
    real(dp), allocatable :: pet_fac_aspect_cache(:) !< cached PET aspect correction (nCells1)
    real(dp), allocatable :: pet_coeff_hs_cache(:) !< cached Hargreaves-Samani coefficient (nCells1)
    real(dp), allocatable :: pet_coeff_pt_cache(:, :) !< cached Priestley-Taylor coefficient (nCells1, nLAI)
    real(dp), allocatable :: pet_fac_lai_cache(:, :, :) !< cached PET-LAI correction (nCells1, nLAI, nLC)
    real(dp), allocatable :: resist_aero_cache(:, :, :) !< cached aerodynamic resistance (nCells1, nLAI, nLC)
    real(dp), allocatable :: resist_surf_cache(:, :) !< cached bulk surface resistance (nCells1, nLAI)
  end type mpr_pet_state_t

  type :: mpr_soil_state_t
    real(dp), allocatable :: sm_exponent_cache(:, :, :) !< cached soil moisture exponent (nCells1, nHorizons, nLC)
    real(dp), allocatable :: sm_saturation_cache(:, :, :) !< cached saturated soil moisture (nCells1, nHorizons, nLC)
    real(dp), allocatable :: sm_field_capacity_cache(:, :, :) !< cached soil moisture field capacity (nCells1, nHorizons, nLC)
    real(dp), allocatable :: wilting_point_cache(:, :, :) !< cached wilting point (nCells1, nHorizons, nLC)
    real(dp), allocatable :: f_roots_cache(:, :, :) !< cached root fractions (nCells1, nHorizons, nLC)
    real(dp), allocatable :: thresh_jarvis_cache(:) !< cached Jarvis threshold field (nCells1)
    real(dp), allocatable :: horizon_bounds(:) !< cached soil-horizon boundaries for restart metadata (nHorizons+1)
    real(dp), allocatable :: sm_deficit_fc_l0(:, :) !< cached saturation deficit from field capacity on L0 (nCells0, nLC)
    real(dp), allocatable :: ks_var_h_l0(:, :) !< cached horizontal Ks variability on L0 (nCells0, nLC)
    real(dp), allocatable :: ks_var_v_l0(:, :) !< cached vertical Ks variability on L0 (nCells0, nLC)
  end type mpr_soil_state_t

  type :: mpr_neutron_state_t
    real(dp), allocatable :: desilets_n0_cache(:) !< cached dry neutron count (nCells1)
    real(dp), allocatable :: bulk_density_cache(:, :, :) !< cached bulk density (nCells1, nHorizons, nLC)
    real(dp), allocatable :: lattice_water_cache(:, :, :) !< cached lattice water (nCells1, nHorizons, nLC)
    real(dp), allocatable :: cosmic_l3_cache(:, :, :) !< cached COSMIC L3 parameter (nCells1, nHorizons, nLC)
  end type mpr_neutron_state_t

  type :: mpr_runoff_state_t
    real(dp), allocatable :: alpha_cache(:, :) !< cached alpha field (nCells1, nLC)
    real(dp), allocatable :: k_fastflow_cache(:, :) !< cached fast interflow recession (nCells1, nLC)
    real(dp), allocatable :: k_slowflow_cache(:, :) !< cached slow interflow recession (nCells1, nLC)
    real(dp), allocatable :: k_baseflow_cache(:, :) !< cached baseflow recession (nCells1, nLC)
    real(dp), allocatable :: k_percolation_cache(:, :) !< cached percolation coefficient (nCells1, nLC)
    real(dp), allocatable :: f_karst_loss_cache(:) !< cached karst loss field (nCells1)
    real(dp), allocatable :: thresh_unsat_cache(:) !< cached unsaturated storage threshold (nCells1)
    real(dp), allocatable :: thresh_sealed_cache(:) !< cached sealed runoff threshold (nCells1)
  end type mpr_runoff_state_t

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
    logical :: read_restart = .false. !< whether to read MPR restart file
    logical :: write_restart = .false. !< whether to write MPR restart file
    character(:), allocatable :: restart_input_path !< path to restart file to read
    character(:), allocatable :: restart_output_path !< path to restart file to write
    real(dp), allocatable :: slope_emp(:) !< empirical slope distribution on level0
    real(dp), allocatable :: lai_l0_cache(:, :) !< cached LAI on level0 (nCells0, nLAI)
    real(dp), allocatable :: max_interception_cache(:, :, :) !< cached max interception (nCells1, nLAI, nLC)
    type(mpr_land_cover_state_t) :: land_cover !< land-cover configuration and cached temporal state
    type(mpr_snow_state_t) :: snow !< cached snow parameter fields
    type(mpr_pet_state_t) :: pet !< cached PET parameter fields
    type(mpr_soil_state_t) :: soil !< cached soil moisture parameter fields
    type(mpr_neutron_state_t) :: neutron !< cached neutron parameter fields
    type(mpr_runoff_state_t) :: runoff !< cached runoff/baseflow parameter fields
    integer(i4) :: n_lai_periods = 1_i4 !< number of cached LAI periods
    integer(i4) :: active_lai_idx = 0_i4 !< active cached LAI index
  contains
    procedure :: configure  => mpr_configure
    procedure :: connect    => mpr_connect
    procedure :: initialize => mpr_initialize
    procedure :: update     => mpr_update
    procedure :: finalize   => mpr_finalize
    procedure, private :: create_restart                  => mpr_create_restart
    procedure, private :: write_restart_data              => mpr_write_restart_data
    procedure, private :: write_restart_land_cover_timing => mpr_write_restart_land_cover_timing
    procedure, private :: read_restart_land_cover_timing  => mpr_read_restart_land_cover_timing
    procedure, private :: read_restart_field_2d           => mpr_read_restart_field_2d
    procedure, private :: read_restart_field_3d           => mpr_read_restart_field_3d
    procedure, private :: read_restart_field_4d           => mpr_read_restart_field_4d
    procedure, private :: write_restart_field_2d          => mpr_write_restart_field_2d
    procedure, private :: write_restart_field_3d          => mpr_write_restart_field_3d
    procedure, private :: write_restart_field_4d          => mpr_write_restart_field_4d
    procedure, private :: read_restart_data               => mpr_read_restart_data
    procedure, private :: ensure_level1_grid              => mpr_ensure_level1_grid
    procedure, private :: init_upscaler                   => mpr_init_upscaler
    procedure, private :: init_slope_emp                  => mpr_init_slope_emp
    procedure, private :: init_temporal_cache             => mpr_init_temporal_cache
    procedure, private :: init_land_cover_timing          => mpr_init_land_cover_timing
    procedure, private :: init_land_cover_cache           => mpr_init_land_cover_cache
    procedure, private :: init_land_cover_fraction_cache  => mpr_init_land_cover_fraction_cache
    procedure, private :: build_lai_l0_cache              => mpr_build_lai_l0_cache
    procedure, private :: load_process_params             => mpr_load_process_params
    procedure, private :: init_max_interception_cache     => mpr_init_max_interception_cache
    procedure, private :: init_snow_cache                 => mpr_init_snow_cache
    procedure, private :: init_pet_cache                  => mpr_init_pet_cache
    procedure, private :: init_pet_aspect_cache           => mpr_init_pet_aspect_cache
    procedure, private :: init_pet_hargreaves_cache       => mpr_init_pet_hargreaves_cache
    procedure, private :: init_pet_lai_cache              => mpr_init_pet_lai_cache
    procedure, private :: init_pet_priestley_taylor_cache => mpr_init_pet_priestley_taylor_cache
    procedure, private :: init_pet_penman_cache           => mpr_init_pet_penman_cache
    procedure, private :: init_soil_cache                 => mpr_init_soil_cache
    procedure, private :: init_runoff_cache               => mpr_init_runoff_cache
    procedure, private :: update_exchange_slices          => mpr_update_exchange_slices
    procedure, private :: lai_index_for_time              => mpr_lai_index_for_time
    procedure, private :: land_cover_index_for_time       => mpr_land_cover_index_for_time
    procedure, private :: check_geo_units_against_lut     => mpr_check_geo_units_against_lut
    procedure, private :: check_geoparameter_consistency  => mpr_check_geoparameter_consistency
    procedure, private :: validate_restart_grid           => mpr_validate_restart_grid
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

    self%land_cover%var_name = trim(self%config%land_cover_var(id(1)))
    if (len_trim(self%land_cover%var_name) < 1) then
      log_fatal(*) "MPR: land_cover_var must not be empty for domain ", n2s(id(1)), "."
      error stop 1
    end if
    status = self%config%is_set("land_cover_path", idx=id, errmsg=errmsg)
    if (status == NML_OK) then
      self%land_cover%path = self%exchange%get_path(self%config%land_cover_path(id(1)))
    else if (.not.self%config%read_restart(id(1))) then
      log_fatal(*) "MPR: land_cover_path not set for domain ", n2s(id(1)), ". Error: ", trim(errmsg)
      error stop 1
    end if

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
    integer(i4) :: pet_process
    integer :: status
    character(1024) :: errmsg
    log_info(*) "Connect MPR"

    id(1) = self%exchange%domain

    ! restart settings
    self%read_restart = self%config%read_restart(id(1))
    self%write_restart = self%config%write_restart(id(1))
    if (self%read_restart) then
      status = self%config%is_set("restart_input_path", idx=id, errmsg=errmsg)
      if (status /= NML_OK) then
        log_fatal(*) "MPR restart input path not set for domain ", n2s(id(1)), ". Error: ", trim(errmsg)
        error stop 1
      end if
      self%restart_input_path = self%exchange%get_path(self%config%restart_input_path(id(1)))
    end if
    if (self%write_restart) then
      status = self%config%is_set("restart_output_path", idx=id, errmsg=errmsg)
      if (status /= NML_OK) then
        log_fatal(*) "MPR restart output path not set for domain ", n2s(id(1)), ". Error: ", trim(errmsg)
        error stop 1
      end if
      self%restart_output_path = self%exchange%get_path(self%config%restart_output_path(id(1)))
    end if

    ! declare MPR prerequisites in the exchange contract
    self%exchange%slope%required = .not.self%read_restart
    self%exchange%aspect%required = .not.self%read_restart
    self%exchange%soil_id%required = .not.self%read_restart
    self%exchange%geo_unit%required = .not.self%read_restart
    pet_process = self%exchange%parameters%process_matrix(5, 1)
    require_gridded_lai = self%config%lai_time_step(id(1)) /= 0_i4 .or. &
      self%exchange%parameters%process_matrix(1, 1) /= 0_i4 .or. &
      pet_process == -1_i4 .or. pet_process == 2_i4 .or. pet_process == 3_i4
    self%exchange%gridded_lai%required = require_gridded_lai .and. .not.self%read_restart

    if (self%read_restart) then
      call self%read_restart_data()
      call self%update_exchange_slices(force=.true.)
      return
    end if

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
          log_fatal(*) "MPR: soil_db_mode=0 expects a single soil class layer, but got ", soil_layers, "."
          error stop 1
        end if
      case (1_i4)
        if (self%config%n_horizons(id(1)) < 1_i4) then
          log_fatal(*) "MPR: n_horizons must be >= 1 for soil_db_mode=1."
          error stop 1
        end if
        if (soil_layers < self%config%n_horizons(id(1))) then
          log_fatal(*) "MPR: soil horizon input provides ", soil_layers, " layers, but n_horizons=", &
            self%config%n_horizons(id(1)), "."
          error stop 1
        end if
      case default
        log_fatal(*) "MPR: unsupported soil_db_mode=", self%config%soil_db_mode(id(1)), "."

        error stop 1
    end select

    if (require_gridded_lai) then
      if (.not.self%exchange%gridded_lai%provided) then
        log_fatal(*) "MPR: gridded_lai not provided, but required for lai_time_step=", self%config%lai_time_step(id(1)), "."
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
      log_fatal(*) "MPR: slope size (", size(self%exchange%slope%data), &
        ") does not match level0 nCells (", int(self%exchange%level0%nCells, i4), ")."
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

  !> \brief Create an MPR restart file.
  subroutine mpr_create_restart(self)
    class(mpr_t), intent(inout), target :: self
    type(NcDataset) :: nc
    type(NcVariable) :: nc_var
    type(NcDimension) :: dims0(0)

    if (.not.allocated(self%restart_output_path)) then
      log_fatal(*) "MPR: restart output path is not configured."
      error stop 1
    end if
    if (.not.associated(self%exchange%level1)) then
      log_fatal(*) "MPR: cannot write restart without a connected level1 grid."
      error stop 1
    end if

    log_info(*) "Write MPR restart to file: ", self%restart_output_path
    nc = NcDataset(self%restart_output_path, "w")
    call self%exchange%level1%to_restart(nc)
    call self%write_restart_data(nc)

    nc_var = nc%setVariable("mpr_meta", "i32", dims0(:0))
    call nc_var%setAttribute("time_stamp", self%exchange%time%str())
    call nc_var%setAttribute("domain", self%exchange%domain)
    call nc_var%setAttribute("n_lai_periods", self%n_lai_periods)
    call nc_var%setAttribute("n_land_cover_periods", self%land_cover%n_periods)
    call nc%close()
  end subroutine mpr_create_restart

  !> \brief Write currently cached MPR effective parameters as unpacked L1 restart data.
  subroutine mpr_write_restart_data(self, nc)
    class(mpr_t), intent(inout), target :: self
    type(NcDataset), intent(inout) :: nc
    type(NcDimension) :: dims_xy(2)
    type(NcDimension) :: lai_dim
    type(NcDimension) :: land_cover_dim
    type(NcDimension) :: soil_dim
    real(dp), allocatable :: soil_bounds(:)
    integer(i4) :: process_case
    integer(i4) :: i

    if (.not.associated(self%exchange%level1)) then
      log_fatal(*) "MPR: level1 grid not connected while writing restart."
      error stop 1
    end if

    if ( self%exchange%level1%coordsys == cartesian ) then
      dims_xy(1) = nc%getDimension("x")
      dims_xy(2) = nc%getDimension("y")
    else
      dims_xy(1) = nc%getDimension("lon")
      dims_xy(2) = nc%getDimension("lat")
    end if
    lai_dim = nc%setDimension(trim(LAIVarName), self%n_lai_periods)
    land_cover_dim = nc%setDimension(trim(landCoverPeriodsVarName), self%land_cover%n_periods)
    call self%write_restart_land_cover_timing(nc, land_cover_dim)
    if (allocated(self%soil%horizon_bounds)) then
      allocate(soil_bounds(size(self%soil%horizon_bounds)))
      soil_bounds = self%soil%horizon_bounds
    else
      allocate(soil_bounds(self%config%n_horizons(self%exchange%domain) + 1_i4))
      do i = 1_i4, size(soil_bounds)
        soil_bounds(i) = real(i - 1_i4, dp)
      end do
    end if
    soil_dim = nc%setCoordinate(trim(soilHorizonsVarName), self%config%n_horizons(self%exchange%domain), &
      bounds=soil_bounds, reference=2_i4)
    deallocate(soil_bounds)

    if (allocated(self%land_cover%sealed_fraction_l1)) then
      call self%write_restart_field_3d( &
        nc, dims_xy, land_cover_dim, "L1_fSealed", "fraction of Sealed area at level 1", &
        self%land_cover%sealed_fraction_l1)
    end if

    if (allocated(self%max_interception_cache)) then
      call self%write_restart_field_3d( &
        nc, dims_xy, lai_dim, "L1_maxInter", "Maximum interception at level 1", &
        self%max_interception_cache(:, :, 1))
    end if

    if (allocated(self%snow%thresh_temp_cache)) then
      call self%write_restart_field_3d( &
        nc, dims_xy, land_cover_dim, "L1_tempThresh", "Threshold temperature for snow/rain at level 1", &
        self%snow%thresh_temp_cache)
      call self%write_restart_field_3d( &
        nc, dims_xy, land_cover_dim, "L1_degDayNoPre", &
        "Degree-day factor with no precipitation at level 1", self%snow%degday_dry_cache)
      call self%write_restart_field_3d( &
        nc, dims_xy, land_cover_dim, "L1_degDayInc", &
        "Increase of the degree-day factor per mm precipitation at level 1", self%snow%degday_inc_cache)
      call self%write_restart_field_3d( &
        nc, dims_xy, land_cover_dim, "L1_degDayMax", &
        "Maximum degree-day factor at level 1", self%snow%degday_max_cache)
    end if

    if (allocated(self%pet%pet_fac_lai_cache)) then
      call self%write_restart_field_4d( &
        nc, dims_xy, lai_dim, land_cover_dim, "L1_petLAIcorFactor", &
        "PET correction factor based on LAI at level 1", self%pet%pet_fac_lai_cache)
    end if
    if (allocated(self%pet%pet_fac_aspect_cache)) then
      call self%write_restart_field_2d( &
        nc, dims_xy, "L1_fAsp", "PET correction factor due to terrain aspect at level 1", &
        self%pet%pet_fac_aspect_cache)
    end if
    if (allocated(self%pet%pet_coeff_hs_cache)) then
      call self%write_restart_field_2d( &
        nc, dims_xy, "L1_HarSamCoeff", "Hargreaves-Samani coefficient", self%pet%pet_coeff_hs_cache)
    end if
    if (allocated(self%pet%pet_coeff_pt_cache)) then
      call self%write_restart_field_3d( &
        nc, dims_xy, lai_dim, "L1_PrieTayAlpha", "Priestley Taylor coefficient (alpha)", &
        self%pet%pet_coeff_pt_cache)
    end if
    if (allocated(self%pet%resist_aero_cache)) then
      call self%write_restart_field_4d( &
        nc, dims_xy, lai_dim, land_cover_dim, "L1_aeroResist", "aerodynamical resistance", &
        self%pet%resist_aero_cache)
    end if
    if (allocated(self%pet%resist_surf_cache)) then
      call self%write_restart_field_3d( &
        nc, dims_xy, lai_dim, "L1_surfResist", "bulk surface resistance", self%pet%resist_surf_cache)
    end if

    if (allocated(self%soil%f_roots_cache)) then
      call self%write_restart_field_4d( &
        nc, dims_xy, soil_dim, land_cover_dim, "L1_fRoots", &
        "Fraction of roots in soil horizons at level 1", self%soil%f_roots_cache)
    end if
    if (allocated(self%soil%sm_saturation_cache)) then
      call self%write_restart_field_4d( &
        nc, dims_xy, soil_dim, land_cover_dim, "L1_soilMoistSat", &
        "Saturation soil moisture for each horizon [mm] at level 1", self%soil%sm_saturation_cache)
    end if
    if (allocated(self%soil%sm_exponent_cache)) then
      call self%write_restart_field_4d( &
        nc, dims_xy, soil_dim, land_cover_dim, "L1_soilMoistExp", &
        "Exponential parameter to how non-linear is the soil water retention at level 1", &
        self%soil%sm_exponent_cache)
    end if
    if (allocated(self%soil%sm_field_capacity_cache)) then
      call self%write_restart_field_4d( &
        nc, dims_xy, soil_dim, land_cover_dim, "L1_soilMoistFC", &
        "SM below which actual ET is reduced linearly till PWP at level 1 for processCase(3)=1", &
        self%soil%sm_field_capacity_cache)
    end if
    if (allocated(self%soil%wilting_point_cache)) then
      call self%write_restart_field_4d( &
        nc, dims_xy, soil_dim, land_cover_dim, "L1_wiltingPoint", &
        "Permanent wilting point at level 1", self%soil%wilting_point_cache)
    end if
    if (allocated(self%runoff%alpha_cache)) then
      call self%write_restart_field_3d( &
        nc, dims_xy, land_cover_dim, "L1_alpha", "Exponent for the upper reservoir at level 1", &
        self%runoff%alpha_cache)
    end if
    if (allocated(self%runoff%f_karst_loss_cache)) then
      call self%write_restart_field_2d( &
        nc, dims_xy, "L1_karstLoss", "Karstic percolation loss at level 1", self%runoff%f_karst_loss_cache)
    end if
    if (allocated(self%runoff%k_fastflow_cache)) then
      call self%write_restart_field_3d( &
        nc, dims_xy, land_cover_dim, "L1_kfastFlow", &
        "Fast interflow recession coefficient at level 1", self%runoff%k_fastflow_cache)
    end if
    if (allocated(self%runoff%k_slowflow_cache)) then
      call self%write_restart_field_3d( &
        nc, dims_xy, land_cover_dim, "L1_kSlowFlow", &
        "Slow interflow recession coefficient at level 1", self%runoff%k_slowflow_cache)
    end if
    if (allocated(self%runoff%k_baseflow_cache)) then
      call self%write_restart_field_3d( &
        nc, dims_xy, land_cover_dim, "L1_kBaseFlow", &
        "Baseflow recession coefficient at level 1", self%runoff%k_baseflow_cache)
    end if
    if (allocated(self%runoff%k_percolation_cache)) then
      call self%write_restart_field_3d( &
        nc, dims_xy, land_cover_dim, "L1_kPerco", &
        "Percolation coefficient at level 1", self%runoff%k_percolation_cache)
    end if
    if (allocated(self%runoff%thresh_unsat_cache)) then
      call self%write_restart_field_2d( &
        nc, dims_xy, "L1_unsatThresh", &
        "Threshold water depth controlling fast interflow at level 1", self%runoff%thresh_unsat_cache)
    end if
    if (allocated(self%runoff%thresh_sealed_cache)) then
      call self%write_restart_field_2d( &
        nc, dims_xy, "L1_sealedThresh", &
        "Threshold water depth for runoff on sealed surfaces at level 1", self%runoff%thresh_sealed_cache)
    end if
    if (allocated(self%neutron%desilets_n0_cache)) then
      call self%write_restart_field_2d( &
        nc, dims_xy, "L1_No_Count", "N0 count at level 1", self%neutron%desilets_n0_cache)
    end if
    if (allocated(self%neutron%bulk_density_cache)) then
      call self%write_restart_field_4d( &
        nc, dims_xy, soil_dim, land_cover_dim, "L1_bulkDens", &
        "Bulk density at level 1 for processCase(10)", self%neutron%bulk_density_cache)
    end if
    if (allocated(self%neutron%lattice_water_cache)) then
      call self%write_restart_field_4d( &
        nc, dims_xy, soil_dim, land_cover_dim, "L1_latticeWater", &
        "Lattice water content at level 1 for processCase(10)", self%neutron%lattice_water_cache)
    end if
    if (allocated(self%neutron%cosmic_l3_cache)) then
      call self%write_restart_field_4d( &
        nc, dims_xy, soil_dim, land_cover_dim, "L1_COSMICL3", &
        "COSMIC L3 parameter at level 1 for processCase(10)", self%neutron%cosmic_l3_cache)
    end if

    process_case = self%exchange%parameters%process_matrix(3, 1)
    if (allocated(self%soil%thresh_jarvis_cache) .and. any(process_case == [2_i4, 3_i4])) then
      call self%write_restart_field_2d( &
        nc, dims_xy, "L1_jarvis_thresh_c1", &
        "jarvis critical value for normalized soil water content", self%soil%thresh_jarvis_cache)
    end if

  end subroutine mpr_write_restart_data

  !> \brief Build the CF units string for temporal land-cover restart metadata.
  function mpr_land_cover_restart_units(start_time) result(units)
    type(datetime), intent(in) :: start_time
    character(:), allocatable :: units

    units = "hours since " // start_time%str()
  end function mpr_land_cover_restart_units

  !> \brief Convert a datetime to an integer hour stamp in CF units.
  integer(i4) function mpr_restart_hour_stamp(ref_time, current_time) result(stamp)
    type(datetime), intent(in) :: ref_time
    type(datetime), intent(in) :: current_time
    real(dp) :: hours_dp
    real(dp) :: rounded_dp

    hours_dp = (current_time - ref_time) / one_hour()
    if (abs(hours_dp) > real(huge(0_i4), dp)) then
      log_fatal(*) "MPR restart: hour offset from ", ref_time%str(), " exceeds i32 range."
      error stop 1
    end if
    rounded_dp = real(nint(hours_dp, kind=i4), dp)
    if (.not.is_close(hours_dp, rounded_dp)) then
      log_fatal(*) "MPR restart: time ", current_time%str(), &
        " is not representable as a whole-hour offset from ", ref_time%str(), "."
      error stop 1
    end if
    stamp = nint(hours_dp, kind=i4)
  end function mpr_restart_hour_stamp

  !> \brief Convert an integer restart time value in CF units to datetime.
  type(datetime) function mpr_datetime_from_restart_time(units, value) result(dt)
    character(*), intent(in) :: units
    integer(i4), intent(in) :: value

    dt = datetime(trim(units), value)
  end function mpr_datetime_from_restart_time

  !> \brief Write temporal land-cover restart timing metadata as an integer CF time axis with bounds.
  subroutine mpr_write_restart_land_cover_timing(self, nc, land_cover_dim)
    class(mpr_t), intent(inout), target :: self
    type(NcDataset), intent(inout) :: nc
    type(NcDimension), intent(in) :: land_cover_dim
    type(NcDimension) :: bounds_dim
    type(NcVariable) :: coord_var
    type(NcVariable) :: bounds_var
    character(:), allocatable :: units
    integer(i4), allocatable :: coord_data(:)
    integer(i4), allocatable :: bounds_data(:, :)
    integer(i4) :: i

    if (.not.self%land_cover%temporal) return
    if (.not.allocated(self%land_cover%period_start) .or. .not.allocated(self%land_cover%period_end)) then
      log_fatal(*) "MPR: temporal land-cover bounds are not cached while writing restart."
      error stop 1
    end if
    if (size(self%land_cover%period_start) /= self%land_cover%n_periods .or. &
      size(self%land_cover%period_end) /= self%land_cover%n_periods) then
      log_fatal(*) "MPR: cached temporal land-cover bounds are inconsistent with n_land_cover_periods."
      error stop 1
    end if

    units = mpr_land_cover_restart_units(self%land_cover%period_start(1))
    allocate(coord_data(self%land_cover%n_periods))
    allocate(bounds_data(2, self%land_cover%n_periods))
    do i = 1_i4, self%land_cover%n_periods
      coord_data(i) = mpr_restart_hour_stamp(self%land_cover%period_start(1), self%land_cover%period_start(i))
      bounds_data(1, i) = coord_data(i)
      bounds_data(2, i) = mpr_restart_hour_stamp(self%land_cover%period_start(1), self%land_cover%period_end(i))
    end do

    coord_var = nc%setVariable(trim(landCoverPeriodsVarName), "i32", [land_cover_dim])
    call coord_var%setAttribute("standard_name", "time")
    call coord_var%setAttribute("axis", "T")
    call coord_var%setAttribute("units", trim(units))
    call coord_var%setAttribute("calendar", lc_restart_calendar)
    call coord_var%setAttribute("bounds", trim(landCoverPeriodsVarName) // "_bnds")
    call coord_var%setData(coord_data)

    if (nc%hasDimension("bnds")) then
      bounds_dim = nc%getDimension("bnds")
    else
      bounds_dim = nc%setDimension("bnds", 2_i4)
    end if
    bounds_var = nc%setVariable(trim(landCoverPeriodsVarName) // "_bnds", "i32", [bounds_dim, land_cover_dim])
    call bounds_var%setData(bounds_data)

    deallocate(coord_data)
    deallocate(bounds_data)
  end subroutine mpr_write_restart_land_cover_timing

  !> \brief Read land-cover time bounds from the restart file and rebuild cached timing state.
  subroutine mpr_read_restart_land_cover_timing(self, nc)
    class(mpr_t), intent(inout), target :: self
    type(NcDataset), intent(inout) :: nc
    type(NcDimension) :: nc_dim
    type(NcVariable) :: nc_var
    integer(i4), allocatable :: var_shape(:)
    integer(i4), allocatable :: coord_values(:)
    integer(i4), allocatable :: bounds_in(:, :)
    integer(i4), allocatable :: bounds_2d(:, :)
    character(256) :: bounds_name
    character(256) :: units
    character(256) :: calendar
    integer(i4) :: n_land_cover_restart
    integer(i4) :: i

    if (.not.nc%hasDimension(trim(landCoverPeriodsVarName))) then
      log_fatal(*) "MPR restart: required land-cover dimension missing: ", trim(landCoverPeriodsVarName)
      error stop 1
    end if
    nc_dim = nc%getDimension(trim(landCoverPeriodsVarName))
    n_land_cover_restart = nc_dim%getLength()
    if (n_land_cover_restart < 1_i4) then
      log_fatal(*) "MPR restart: land-cover period count must be >= 1."
      error stop 1
    end if

    if (allocated(self%land_cover%period_start)) deallocate(self%land_cover%period_start)
    if (allocated(self%land_cover%period_end)) deallocate(self%land_cover%period_end)
    self%land_cover%n_periods = n_land_cover_restart
    self%land_cover%temporal = .false.

    if (.not.nc%hasVariable(trim(landCoverPeriodsVarName))) then
      if (nc%hasVariable(trim(landCoverPeriodsVarName) // "_bnds")) then
        log_fatal(*) "MPR restart: static land-cover restart must not carry temporal bounds metadata."
        error stop 1
      end if
      if (n_land_cover_restart /= 1_i4) then
        log_fatal(*) "MPR restart: land-cover dimension length ", n2s(n_land_cover_restart), &
          " requires temporal land-cover metadata."
        error stop 1
      end if
      return
    end if

    nc_var = nc%getVariable(trim(landCoverPeriodsVarName))
    if (trim(nc_var%getDtype()) /= "i32") then
      log_fatal(*) "MPR restart: land-cover time coordinate must use i32 CF time values."
      error stop 1
    end if
    if (.not.nc_var%hasAttribute("units") .or. .not.nc_var%hasAttribute("bounds")) then
      log_fatal(*) "MPR restart: ", trim(landCoverPeriodsVarName), &
        " must use the new time-bounded restart format; re-write the restart with the updated MPR container."
      error stop 1
    end if
    call nc_var%getAttribute("units", units)
    if (index(trim(units), "hours since ") /= 1) then
      log_fatal(*) "MPR restart: unsupported land-cover time units '", trim(units), "'."
      error stop 1
    end if
    if (.not.nc_var%hasAttribute("calendar")) then
      log_fatal(*) "MPR restart: ", trim(landCoverPeriodsVarName), " is missing the calendar attribute."
      error stop 1
    end if
    call nc_var%getAttribute("calendar", calendar)
    if (trim(calendar) /= lc_restart_calendar) then
      log_fatal(*) "MPR restart: unsupported land-cover calendar '", trim(calendar), "'."
      error stop 1
    end if
    call nc_var%getAttribute("bounds", bounds_name)
    bounds_name = trim(bounds_name)
    if (len_trim(bounds_name) < 1) then
      log_fatal(*) "MPR restart: land-cover time coordinate has an empty bounds attribute."
      error stop 1
    end if
    if (.not.nc%hasVariable(bounds_name)) then
      log_fatal(*) "MPR restart: required land-cover bounds variable missing: ", trim(bounds_name)
      error stop 1
    end if
    call nc_var%getData(coord_values)
    if (size(coord_values) /= n_land_cover_restart) then
      log_fatal(*) "MPR restart: land-cover time coordinate length does not match its dimension."
      error stop 1
    end if

    nc_var = nc%getVariable(bounds_name)
    if (trim(nc_var%getDtype()) /= "i32") then
      log_fatal(*) "MPR restart: land-cover bounds variable must use i32 CF time values."
      error stop 1
    end if
    var_shape = nc_var%getShape()
    if (size(var_shape) /= 2_i4) then
      log_fatal(*) "MPR restart: land-cover bounds variable ", trim(bounds_name), " has rank ", &
        n2s(size(var_shape)), ", expected 2."
      error stop 1
    end if
    call nc_var%getData(bounds_in)
    if (size(bounds_in, 1) == 2_i4 .and. size(bounds_in, 2) == n_land_cover_restart) then
      allocate(bounds_2d(2, n_land_cover_restart))
      bounds_2d = bounds_in
    else if (size(bounds_in, 2) == 2_i4 .and. size(bounds_in, 1) == n_land_cover_restart) then
      allocate(bounds_2d(2, n_land_cover_restart))
      bounds_2d = transpose(bounds_in)
    else
      log_fatal(*) "MPR restart: land-cover bounds variable ", trim(bounds_name), " has incompatible shape."
      error stop 1
    end if
    deallocate(bounds_in)

    allocate(self%land_cover%period_start(n_land_cover_restart))
    allocate(self%land_cover%period_end(n_land_cover_restart))
    do i = 1_i4, n_land_cover_restart
      if (bounds_2d(2, i) <= bounds_2d(1, i)) then
        log_fatal(*) "MPR restart: invalid land-cover time bounds for period ", n2s(i), "."
        error stop 1
      end if
      if (coord_values(i) /= bounds_2d(1, i)) then
        log_fatal(*) "MPR restart: land-cover time coordinate does not match lower bounds for period ", n2s(i), "."
        error stop 1
      end if
      self%land_cover%period_start(i) = mpr_datetime_from_restart_time(trim(units), bounds_2d(1, i))
      self%land_cover%period_end(i) = mpr_datetime_from_restart_time(trim(units), bounds_2d(2, i))
    end do
    deallocate(coord_values)
    deallocate(bounds_2d)

    if (self%exchange%start_time < self%land_cover%period_start(1)) then
      log_fatal(*) "MPR restart: land-cover time bounds start at ", self%land_cover%period_start(1)%str(), &
        ", but simulation starts at ", self%exchange%start_time%str(), "."
      error stop 1
    end if
    if (self%exchange%end_time > self%land_cover%period_end(n_land_cover_restart)) then
      log_fatal(*) "MPR restart: land-cover time bounds end at ", self%land_cover%period_end(n_land_cover_restart)%str(), &
        ", but simulation ends at ", self%exchange%end_time%str(), "."
      error stop 1
    end if

    self%land_cover%temporal = .true.
  end subroutine mpr_read_restart_land_cover_timing

  !> \brief Read a packed L1 scalar field from unpacked 2D restart data.
  subroutine mpr_read_restart_field_2d(self, nc, var_name, data_packed)
    class(mpr_t), intent(inout), target :: self
    type(NcDataset), intent(in) :: nc
    character(*), intent(in) :: var_name
    real(dp), allocatable, intent(out) :: data_packed(:)
    type(NcVariable) :: nc_var
    integer(i4), allocatable :: var_shape(:)
    real(dp), allocatable :: data_2d(:, :)

    if (.not.nc%hasVariable(trim(var_name))) then
      log_fatal(*) "MPR restart: required variable missing: ", trim(var_name)
      error stop 1
    end if
    nc_var = nc%getVariable(trim(var_name))
    var_shape = nc_var%getShape()
    if (size(var_shape) /= 2) then
      log_fatal(*) "MPR restart: variable ", trim(var_name), " has rank ", n2s(size(var_shape)), ", expected 2."
      error stop 1
    end if
    if (any(var_shape /= [self%exchange%level1%nx, self%exchange%level1%ny])) then
      log_fatal(*) "MPR restart: variable ", trim(var_name), " has incompatible x/y shape."
      error stop 1
    end if
    call nc_var%getData(data_2d)
    allocate(data_packed(self%exchange%level1%ncells))
    call self%exchange%level1%pack_into(data_2d, data_packed)
  end subroutine mpr_read_restart_field_2d

  !> \brief Read a packed L1 field with one auxiliary dimension from unpacked 3D restart data.
  subroutine mpr_read_restart_field_3d(self, nc, var_name, data_packed)
    class(mpr_t), intent(inout), target :: self
    type(NcDataset), intent(in) :: nc
    character(*), intent(in) :: var_name
    real(dp), allocatable, intent(out) :: data_packed(:, :)
    type(NcVariable) :: nc_var
    integer(i4), allocatable :: var_shape(:)
    real(dp), allocatable :: data_3d(:, :, :)
    integer(i4) :: idx

    if (.not.nc%hasVariable(trim(var_name))) then
      log_fatal(*) "MPR restart: required variable missing: ", trim(var_name)
      error stop 1
    end if
    nc_var = nc%getVariable(trim(var_name))
    var_shape = nc_var%getShape()
    if (size(var_shape) /= 3) then
      log_fatal(*) "MPR restart: variable ", trim(var_name), " has rank ", n2s(size(var_shape)), ", expected 3."
      error stop 1
    end if
    if (any(var_shape(1:2) /= [self%exchange%level1%nx, self%exchange%level1%ny])) then
      log_fatal(*) "MPR restart: variable ", trim(var_name), " has incompatible x/y shape."
      error stop 1
    end if
    call nc_var%getData(data_3d)
    allocate(data_packed(self%exchange%level1%ncells, var_shape(3)))
    do idx = 1_i4, var_shape(3)
      call self%exchange%level1%pack_into(data_3d(:, :, idx), data_packed(:, idx))
    end do
  end subroutine mpr_read_restart_field_3d

  !> \brief Read a packed L1 field with two auxiliary dimensions from unpacked 4D restart data.
  subroutine mpr_read_restart_field_4d(self, nc, var_name, data_packed)
    class(mpr_t), intent(inout), target :: self
    type(NcDataset), intent(in) :: nc
    character(*), intent(in) :: var_name
    real(dp), allocatable, intent(out) :: data_packed(:, :, :)
    type(NcVariable) :: nc_var
    integer(i4), allocatable :: var_shape(:)
    real(dp), allocatable :: data_4d(:, :, :, :)
    integer(i4) :: idx3
    integer(i4) :: idx4

    if (.not.nc%hasVariable(trim(var_name))) then
      log_fatal(*) "MPR restart: required variable missing: ", trim(var_name)
      error stop 1
    end if
    nc_var = nc%getVariable(trim(var_name))
    var_shape = nc_var%getShape()
    if (size(var_shape) /= 4) then
      log_fatal(*) "MPR restart: variable ", trim(var_name), " has rank ", n2s(size(var_shape)), ", expected 4."
      error stop 1
    end if
    if (any(var_shape(1:2) /= [self%exchange%level1%nx, self%exchange%level1%ny])) then
      log_fatal(*) "MPR restart: variable ", trim(var_name), " has incompatible x/y shape."
      error stop 1
    end if
    call nc_var%getData(data_4d)
    allocate(data_packed(self%exchange%level1%ncells, var_shape(3), var_shape(4)))
    do idx4 = 1_i4, var_shape(4)
      do idx3 = 1_i4, var_shape(3)
        call self%exchange%level1%pack_into(data_4d(:, :, idx3, idx4), data_packed(:, idx3, idx4))
      end do
    end do
  end subroutine mpr_read_restart_field_4d

  !> \brief Write a packed L1 scalar field as unpacked 2D restart data.
  subroutine mpr_write_restart_field_2d(self, nc, dims_xy, var_name, long_name, data_packed)
    class(mpr_t), intent(inout), target :: self
    type(NcDataset), intent(inout) :: nc
    type(NcDimension), intent(in) :: dims_xy(2)
    character(*), intent(in) :: var_name
    character(*), intent(in) :: long_name
    real(dp), intent(in) :: data_packed(:)
    type(NcVariable) :: nc_var
    real(dp), allocatable :: data_2d(:, :)

    allocate(data_2d(self%exchange%level1%nx, self%exchange%level1%ny))
    call self%exchange%level1%unpack_into(data_packed, data_2d)
    nc_var = nc%setVariable(trim(var_name), "f64", dims_xy)
    call nc_var%setFillValue(nodata_dp)
    call nc_var%setAttribute("missing_value", nodata_dp)
    call nc_var%setAttribute("long_name", trim(long_name))
    if (self%exchange%level1%has_aux_coords()) call nc_var%setAttribute("coordinates", "lon lat")
    call nc_var%setData(data_2d)
    deallocate(data_2d)
  end subroutine mpr_write_restart_field_2d

  !> \brief Write a packed L1 field with one auxiliary dimension as unpacked 3D restart data.
  subroutine mpr_write_restart_field_3d(self, nc, dims_xy, dim3, var_name, long_name, data_packed)
    class(mpr_t), intent(inout), target :: self
    type(NcDataset), intent(inout) :: nc
    type(NcDimension), intent(in) :: dims_xy(2)
    type(NcDimension), intent(in) :: dim3
    character(*), intent(in) :: var_name
    character(*), intent(in) :: long_name
    real(dp), intent(in) :: data_packed(:, :)
    type(NcDimension) :: dims(3)
    type(NcVariable) :: nc_var
    real(dp), allocatable :: data_3d(:, :, :)
    integer(i4) :: idx

    dims(1:2) = dims_xy
    dims(3) = dim3
    allocate(data_3d(self%exchange%level1%nx, self%exchange%level1%ny, size(data_packed, 2)))
    do idx = 1_i4, size(data_packed, 2)
      call self%exchange%level1%unpack_into(data_packed(:, idx), data_3d(:, :, idx))
    end do
    nc_var = nc%setVariable(trim(var_name), "f64", dims)
    call nc_var%setFillValue(nodata_dp)
    call nc_var%setAttribute("missing_value", nodata_dp)
    call nc_var%setAttribute("long_name", trim(long_name))
    if (self%exchange%level1%has_aux_coords()) call nc_var%setAttribute("coordinates", "lon lat")
    call nc_var%setData(data_3d)
    deallocate(data_3d)
  end subroutine mpr_write_restart_field_3d

  !> \brief Write a packed L1 field with two auxiliary dimensions as unpacked 4D restart data.
  subroutine mpr_write_restart_field_4d(self, nc, dims_xy, dim3, dim4, var_name, long_name, data_packed)
    class(mpr_t), intent(inout), target :: self
    type(NcDataset), intent(inout) :: nc
    type(NcDimension), intent(in) :: dims_xy(2)
    type(NcDimension), intent(in) :: dim3
    type(NcDimension), intent(in) :: dim4
    character(*), intent(in) :: var_name
    character(*), intent(in) :: long_name
    real(dp), intent(in) :: data_packed(:, :, :)
    type(NcDimension) :: dims(4)
    type(NcVariable) :: nc_var
    real(dp), allocatable :: data_4d(:, :, :, :)
    integer(i4) :: idx3
    integer(i4) :: idx4

    dims(1:2) = dims_xy
    dims(3) = dim3
    dims(4) = dim4
    allocate(data_4d(self%exchange%level1%nx, self%exchange%level1%ny, size(data_packed, 2), size(data_packed, 3)))
    do idx4 = 1_i4, size(data_packed, 3)
      do idx3 = 1_i4, size(data_packed, 2)
        call self%exchange%level1%unpack_into(data_packed(:, idx3, idx4), data_4d(:, :, idx3, idx4))
      end do
    end do
    nc_var = nc%setVariable(trim(var_name), "f64", dims)
    call nc_var%setFillValue(nodata_dp)
    call nc_var%setAttribute("missing_value", nodata_dp)
    call nc_var%setAttribute("long_name", trim(long_name))
    if (self%exchange%level1%has_aux_coords()) call nc_var%setAttribute("coordinates", "lon lat")
    call nc_var%setData(data_4d)
    deallocate(data_4d)
  end subroutine mpr_write_restart_field_4d

  subroutine mpr_read_restart_data(self)
    class(mpr_t), intent(inout), target :: self
    type(NcDataset) :: nc
    type(NcDimension) :: nc_dim
    type(NcVariable) :: nc_var
    type(grid_t) :: restart_grid
    integer(i4) :: pet_process
    integer(i4) :: soil_process
    integer(i4) :: neutron_process
    integer(i4) :: n_lai_restart
    integer(i4) :: n_land_cover_restart
    integer(i4) :: n_soil_restart
    integer(i4) :: land_cover_idx
    real(dp), allocatable :: field_3d(:, :)
    real(dp), allocatable :: bounds_2d(:, :)

    if (.not.allocated(self%restart_input_path)) then
      log_fatal(*) "MPR: restart input path is not configured."
      error stop 1
    end if
    if (self%exchange%parameters%process_matrix(10, 1) > 0_i4 .and. self%exchange%parameters%process_matrix(3, 1) == 0_i4) then
      log_fatal(*) "MPR: neutron regionalization requires an active soil-moisture process."
      error stop 1
    end if

    nc = NcDataset(self%restart_input_path, "r")
    call restart_grid%from_restart(nc)
    if (associated(self%exchange%level1)) then
      call self%validate_restart_grid(restart_grid)
    else
      self%tgt_level1 = restart_grid
      self%exchange%level1 => self%tgt_level1
      log_info(*) "MPR restart: bootstrap level1 grid from restart file."
    end if
    call self%read_restart_land_cover_timing(nc)
    n_land_cover_restart = self%land_cover%n_periods

    pet_process = self%exchange%parameters%process_matrix(5, 1)
    soil_process = self%exchange%parameters%process_matrix(3, 1)
    neutron_process = self%exchange%parameters%process_matrix(10, 1)
    if (self%exchange%parameters%process_matrix(1, 1) /= 0_i4 .or. any(pet_process == [-1_i4, 2_i4, 3_i4])) then
      if (.not.nc%hasDimension(trim(LAIVarName))) then
        log_fatal(*) "MPR restart: required LAI dimension missing: ", trim(LAIVarName)
        error stop 1
      end if
      nc_dim = nc%getDimension(trim(LAIVarName))
      n_lai_restart = nc_dim%getLength()
      if (n_lai_restart < 1_i4) then
        log_fatal(*) "MPR restart: LAI dimension length must be >= 1."
        error stop 1
      end if
      self%n_lai_periods = n_lai_restart
    else
      self%n_lai_periods = 1_i4
    end if

    if (soil_process /= 0_i4 .or. neutron_process > 0_i4) then
      if (.not.nc%hasDimension(trim(soilHorizonsVarName))) then
        log_fatal(*) "MPR restart: required soil-horizon dimension missing: ", trim(soilHorizonsVarName)
        error stop 1
      end if
      nc_dim = nc%getDimension(trim(soilHorizonsVarName))
      n_soil_restart = nc_dim%getLength()
      if (n_soil_restart /= self%config%n_horizons(self%exchange%domain)) then
        log_fatal(*) "MPR restart: soil-horizon count ", n2s(n_soil_restart), &
          " does not match current config ", n2s(self%config%n_horizons(self%exchange%domain)), "."
        error stop 1
      end if
      if (allocated(self%soil%horizon_bounds)) deallocate(self%soil%horizon_bounds)
      if (nc%hasVariable(trim(soilHorizonsVarName)//"_bnds")) then
        nc_var = nc%getVariable(trim(soilHorizonsVarName)//"_bnds")
        call nc_var%getData(bounds_2d)
        if (size(bounds_2d, 1) == 2_i4 .and. size(bounds_2d, 2) == n_soil_restart) then
          allocate(self%soil%horizon_bounds(n_soil_restart + 1_i4))
          self%soil%horizon_bounds(1) = bounds_2d(1, 1)
          self%soil%horizon_bounds(2:) = bounds_2d(2, :)
        end if
      end if
    end if

    if (allocated(self%land_cover%sealed_fraction_l1)) deallocate(self%land_cover%sealed_fraction_l1)
    call self%read_restart_field_3d(nc, "L1_fSealed", field_3d)
    if (size(field_3d, 2) /= self%land_cover%n_periods) then
      log_fatal(*) "MPR restart: L1_fSealed land-cover dimension does not match current timing configuration."
      error stop 1
    end if
    allocate(self%land_cover%sealed_fraction_l1(self%exchange%level1%ncells, self%land_cover%n_periods))
    self%land_cover%sealed_fraction_l1 = field_3d
    deallocate(field_3d)
    self%exchange%f_sealed%provided = .true.

    if (self%exchange%parameters%process_matrix(1, 1) /= 0_i4) then
      if (allocated(self%max_interception_cache)) deallocate(self%max_interception_cache)
      call self%read_restart_field_3d(nc, "L1_maxInter", field_3d)
      if (size(field_3d, 2) /= self%n_lai_periods) then
        log_fatal(*) "MPR restart: L1_maxInter LAI dimension does not match restart LAI periods."
        error stop 1
      end if
      allocate(self%max_interception_cache(self%exchange%level1%ncells, self%n_lai_periods, self%land_cover%n_periods))
      do land_cover_idx = 1_i4, self%land_cover%n_periods
        self%max_interception_cache(:, :, land_cover_idx) = field_3d
      end do
      deallocate(field_3d)
      self%exchange%max_interception%provided = .true.
    end if

    if (self%exchange%parameters%process_matrix(2, 1) /= 0_i4) then
      if (allocated(self%snow%thresh_temp_cache)) deallocate(self%snow%thresh_temp_cache)
      if (allocated(self%snow%degday_dry_cache)) deallocate(self%snow%degday_dry_cache)
      if (allocated(self%snow%degday_inc_cache)) deallocate(self%snow%degday_inc_cache)
      if (allocated(self%snow%degday_max_cache)) deallocate(self%snow%degday_max_cache)
      call self%read_restart_field_3d(nc, "L1_tempThresh", self%snow%thresh_temp_cache)
      call self%read_restart_field_3d(nc, "L1_degDayNoPre", self%snow%degday_dry_cache)
      call self%read_restart_field_3d(nc, "L1_degDayInc", self%snow%degday_inc_cache)
      call self%read_restart_field_3d(nc, "L1_degDayMax", self%snow%degday_max_cache)
      if (any([size(self%snow%thresh_temp_cache, 2), size(self%snow%degday_dry_cache, 2), &
        size(self%snow%degday_inc_cache, 2), size(self%snow%degday_max_cache, 2)] /= self%land_cover%n_periods)) then
        log_fatal(*) "MPR restart: snow restart land-cover dimensions do not match current timing configuration."
        error stop 1
      end if
      self%exchange%thresh_temp%provided = .true.
      self%exchange%degday_dry%provided = .true.
      self%exchange%degday_inc%provided = .true.
      self%exchange%degday_max%provided = .true.
    end if

    select case (pet_process)
      case (-2_i4)
        if (allocated(self%pet%pet_fac_aspect_cache)) deallocate(self%pet%pet_fac_aspect_cache)
        call self%read_restart_field_2d(nc, "L1_fAsp", self%pet%pet_fac_aspect_cache)
        self%exchange%pet_fac_aspect%provided = .true.
      case (-1_i4)
        if (allocated(self%pet%pet_fac_lai_cache)) deallocate(self%pet%pet_fac_lai_cache)
        call self%read_restart_field_4d(nc, "L1_petLAIcorFactor", self%pet%pet_fac_lai_cache)
        if (size(self%pet%pet_fac_lai_cache, 2) /= self%n_lai_periods .or. &
          size(self%pet%pet_fac_lai_cache, 3) /= self%land_cover%n_periods) then
          log_fatal(*) "MPR restart: PET-LAI restart dimensions do not match current LAI/land-cover configuration."
          error stop 1
        end if
        self%exchange%pet_fac_lai%provided = .true.
      case (1_i4)
        if (allocated(self%pet%pet_fac_aspect_cache)) deallocate(self%pet%pet_fac_aspect_cache)
        if (allocated(self%pet%pet_coeff_hs_cache)) deallocate(self%pet%pet_coeff_hs_cache)
        call self%read_restart_field_2d(nc, "L1_fAsp", self%pet%pet_fac_aspect_cache)
        call self%read_restart_field_2d(nc, "L1_HarSamCoeff", self%pet%pet_coeff_hs_cache)
        self%exchange%pet_fac_aspect%provided = .true.
        self%exchange%pet_coeff_hs%provided = .true.
      case (2_i4)
        if (allocated(self%pet%pet_coeff_pt_cache)) deallocate(self%pet%pet_coeff_pt_cache)
        call self%read_restart_field_3d(nc, "L1_PrieTayAlpha", self%pet%pet_coeff_pt_cache)
        if (size(self%pet%pet_coeff_pt_cache, 2) /= self%n_lai_periods) then
          log_fatal(*) "MPR restart: Priestley-Taylor restart LAI dimension does not match restart LAI periods."
          error stop 1
        end if
        self%exchange%pet_coeff_pt%provided = .true.
      case (3_i4)
        if (allocated(self%pet%resist_aero_cache)) deallocate(self%pet%resist_aero_cache)
        if (allocated(self%pet%resist_surf_cache)) deallocate(self%pet%resist_surf_cache)
        call self%read_restart_field_4d(nc, "L1_aeroResist", self%pet%resist_aero_cache)
        call self%read_restart_field_3d(nc, "L1_surfResist", self%pet%resist_surf_cache)
        if (size(self%pet%resist_aero_cache, 2) /= self%n_lai_periods .or. &
          size(self%pet%resist_aero_cache, 3) /= self%land_cover%n_periods) then
          log_fatal(*) "MPR restart: aerodynamic resistance dimensions do not match current LAI/land-cover configuration."
          error stop 1
        end if
        if (size(self%pet%resist_surf_cache, 2) /= self%n_lai_periods) then
          log_fatal(*) "MPR restart: surface resistance LAI dimension does not match restart LAI periods."
          error stop 1
        end if
        self%exchange%resist_aero%provided = .true.
        self%exchange%resist_surf%provided = .true.
    end select

    if (soil_process /= 0_i4) then
      if (allocated(self%soil%sm_exponent_cache)) deallocate(self%soil%sm_exponent_cache)
      if (allocated(self%soil%sm_saturation_cache)) deallocate(self%soil%sm_saturation_cache)
      if (allocated(self%soil%sm_field_capacity_cache)) deallocate(self%soil%sm_field_capacity_cache)
      if (allocated(self%soil%wilting_point_cache)) deallocate(self%soil%wilting_point_cache)
      if (allocated(self%soil%f_roots_cache)) deallocate(self%soil%f_roots_cache)
      if (allocated(self%soil%thresh_jarvis_cache)) deallocate(self%soil%thresh_jarvis_cache)
      call self%read_restart_field_4d(nc, "L1_fRoots", self%soil%f_roots_cache)
      call self%read_restart_field_4d(nc, "L1_soilMoistSat", self%soil%sm_saturation_cache)
      call self%read_restart_field_4d(nc, "L1_soilMoistExp", self%soil%sm_exponent_cache)
      call self%read_restart_field_4d(nc, "L1_soilMoistFC", self%soil%sm_field_capacity_cache)
      call self%read_restart_field_4d(nc, "L1_wiltingPoint", self%soil%wilting_point_cache)
      if (any([size(self%soil%f_roots_cache, 2), size(self%soil%sm_saturation_cache, 2), size(self%soil%sm_exponent_cache, 2), &
        size(self%soil%sm_field_capacity_cache, 2), size(self%soil%wilting_point_cache, 2)] /= n_soil_restart) .or. &
        any([size(self%soil%f_roots_cache, 3), size(self%soil%sm_saturation_cache, 3), size(self%soil%sm_exponent_cache, 3), &
        size(self%soil%sm_field_capacity_cache, 3), size(self%soil%wilting_point_cache, 3)] /= self%land_cover%n_periods)) then
        log_fatal(*) "MPR restart: soil-moisture restart dimensions do not match current soil/land-cover configuration."
        error stop 1
      end if
      allocate(self%soil%thresh_jarvis_cache(self%exchange%level1%ncells))
      if (any(soil_process == [2_i4, 3_i4])) then
        call self%read_restart_field_2d(nc, "L1_jarvis_thresh_c1", self%soil%thresh_jarvis_cache)
      else
        self%soil%thresh_jarvis_cache = nodata_dp
      end if
      self%exchange%f_roots%provided = .true.
      self%exchange%sm_saturation%provided = .true.
      self%exchange%sm_exponent%provided = .true.
      self%exchange%sm_field_capacity%provided = .true.
      self%exchange%wilting_point%provided = .true.
      self%exchange%thresh_jarvis%provided = .true.
    end if

    if (self%exchange%parameters%process_matrix(6, 1) /= 0_i4) then
      if (allocated(self%runoff%alpha_cache)) deallocate(self%runoff%alpha_cache)
      if (allocated(self%runoff%k_fastflow_cache)) deallocate(self%runoff%k_fastflow_cache)
      if (allocated(self%runoff%k_slowflow_cache)) deallocate(self%runoff%k_slowflow_cache)
      if (allocated(self%runoff%thresh_unsat_cache)) deallocate(self%runoff%thresh_unsat_cache)
      call self%read_restart_field_3d(nc, "L1_alpha", self%runoff%alpha_cache)
      call self%read_restart_field_3d(nc, "L1_kfastFlow", self%runoff%k_fastflow_cache)
      call self%read_restart_field_3d(nc, "L1_kSlowFlow", self%runoff%k_slowflow_cache)
      call self%read_restart_field_2d(nc, "L1_unsatThresh", self%runoff%thresh_unsat_cache)
      if (any([size(self%runoff%alpha_cache, 2), size(self%runoff%k_fastflow_cache, 2), &
        size(self%runoff%k_slowflow_cache, 2)] /= self%land_cover%n_periods)) then
        log_fatal(*) "MPR restart: runoff restart land-cover dimensions do not match current timing configuration."
        error stop 1
      end if
      self%exchange%alpha%provided = .true.
      self%exchange%k_fastflow%provided = .true.
      self%exchange%k_slowflow%provided = .true.
      self%exchange%thresh_unsat%provided = .true.
    end if
    if (self%exchange%parameters%process_matrix(7, 1) /= 0_i4) then
      if (allocated(self%runoff%k_percolation_cache)) deallocate(self%runoff%k_percolation_cache)
      if (allocated(self%runoff%f_karst_loss_cache)) deallocate(self%runoff%f_karst_loss_cache)
      call self%read_restart_field_3d(nc, "L1_kPerco", self%runoff%k_percolation_cache)
      call self%read_restart_field_2d(nc, "L1_karstLoss", self%runoff%f_karst_loss_cache)
      if (size(self%runoff%k_percolation_cache, 2) /= self%land_cover%n_periods) then
        log_fatal(*) "MPR restart: percolation restart land-cover dimension does not match current timing configuration."
        error stop 1
      end if
      self%exchange%k_percolation%provided = .true.
      self%exchange%f_karst_loss%provided = .true.
    end if
    if (self%exchange%parameters%process_matrix(4, 1) /= 0_i4) then
      if (allocated(self%runoff%thresh_sealed_cache)) deallocate(self%runoff%thresh_sealed_cache)
      call self%read_restart_field_2d(nc, "L1_sealedThresh", self%runoff%thresh_sealed_cache)
      self%exchange%thresh_sealed%provided = .true.
    end if
    if (self%exchange%parameters%process_matrix(9, 1) /= 0_i4) then
      if (allocated(self%runoff%k_baseflow_cache)) deallocate(self%runoff%k_baseflow_cache)
      call self%read_restart_field_3d(nc, "L1_kBaseFlow", self%runoff%k_baseflow_cache)
      if (size(self%runoff%k_baseflow_cache, 2) /= self%land_cover%n_periods) then
        log_fatal(*) "MPR restart: baseflow restart land-cover dimension does not match current timing configuration."
        error stop 1
      end if
      self%exchange%k_baseflow%provided = .true.
    end if

    if (neutron_process > 0_i4) then
      if (allocated(self%neutron%desilets_n0_cache)) deallocate(self%neutron%desilets_n0_cache)
      if (allocated(self%neutron%bulk_density_cache)) deallocate(self%neutron%bulk_density_cache)
      if (allocated(self%neutron%lattice_water_cache)) deallocate(self%neutron%lattice_water_cache)
      if (allocated(self%neutron%cosmic_l3_cache)) deallocate(self%neutron%cosmic_l3_cache)
      call self%read_restart_field_2d(nc, "L1_No_Count", self%neutron%desilets_n0_cache)
      call self%read_restart_field_4d(nc, "L1_bulkDens", self%neutron%bulk_density_cache)
      call self%read_restart_field_4d(nc, "L1_latticeWater", self%neutron%lattice_water_cache)
      if (any([size(self%neutron%bulk_density_cache, 2), size(self%neutron%lattice_water_cache, 2)] /= n_soil_restart) .or. &
        any([size(self%neutron%bulk_density_cache, 3), size(self%neutron%lattice_water_cache, 3)] /= self%land_cover%n_periods)) then
        log_fatal(*) "MPR restart: neutron restart dimensions do not match current soil/land-cover configuration."
        error stop 1
      end if
      if (neutron_process == 2_i4) then
        call self%read_restart_field_4d(nc, "L1_COSMICL3", self%neutron%cosmic_l3_cache)
        if (size(self%neutron%cosmic_l3_cache, 2) /= n_soil_restart .or. &
          size(self%neutron%cosmic_l3_cache, 3) /= self%land_cover%n_periods) then
          log_fatal(*) "MPR restart: COSMIC L3 restart dimensions do not match current soil/land-cover configuration."
          error stop 1
        end if
      end if
      self%exchange%desilets_n0%provided = .true.
      self%exchange%bulk_density%provided = .true.
      self%exchange%lattice_water%provided = .true.
      if (allocated(self%neutron%cosmic_l3_cache)) self%exchange%cosmic_l3%provided = .true.
    end if

    self%active_lai_idx = 0_i4
    self%land_cover%active_idx = 0_i4
    call nc%close()
  end subroutine mpr_read_restart_data

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

  !> \brief Validate that the restart grid matches the currently configured level1 grid.
  subroutine mpr_validate_restart_grid(self, restart_grid)
    class(mpr_t), intent(inout), target :: self
    type(grid_t), intent(in), target :: restart_grid

    if (.not.associated(self%exchange%level1)) then
      log_fatal(*) "MPR restart: level1 grid not connected before restart-grid validation."
      error stop 1
    end if
    if (restart_grid%coordsys /= self%exchange%level1%coordsys) then
      log_fatal(*) "MPR restart: restart grid coordinate system does not match current level1 grid."
      error stop 1
    end if
    if (restart_grid%nx /= self%exchange%level1%nx .or. restart_grid%ny /= self%exchange%level1%ny) then
      log_fatal(*) "MPR restart: restart grid dimensions do not match current level1 grid."
      error stop 1
    end if
    if (.not.is_close(restart_grid%cellsize, self%exchange%level1%cellsize) .or. &
      .not.is_close(restart_grid%xllcorner, self%exchange%level1%xllcorner) .or. &
      .not.is_close(restart_grid%yllcorner, self%exchange%level1%yllcorner)) then
      log_fatal(*) "MPR restart: restart grid geometry does not match current level1 grid."
      error stop 1
    end if
    if (restart_grid%y_direction /= self%exchange%level1%y_direction) then
      log_fatal(*) "MPR restart: restart grid y-direction does not match current level1 grid."
      error stop 1
    end if
    if (.not.allocated(restart_grid%mask) .or. .not.allocated(self%exchange%level1%mask)) then
      log_fatal(*) "MPR restart: mask information missing during restart-grid validation."
      error stop 1
    end if
    if (any(restart_grid%mask .neqv. self%exchange%level1%mask)) then
      log_fatal(*) "MPR restart: restart grid mask does not match current level1 grid."
      error stop 1
    end if
    if (self%exchange%level1%has_aux_coords()) then
      if (.not.restart_grid%has_aux_coords()) then
        log_fatal(*) "MPR restart: current level1 grid has auxiliary coordinates, but restart grid does not."
        error stop 1
      end if
      if (any(.not.is_close(restart_grid%lon, self%exchange%level1%lon)) .or. &
        any(.not.is_close(restart_grid%lat, self%exchange%level1%lat))) then
        log_fatal(*) "MPR restart: restart grid auxiliary coordinates do not match current level1 grid."
        error stop 1
      end if
    end if
  end subroutine mpr_validate_restart_grid

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
    logical :: need_lai_cache
    logical :: need_runoff_cache
    integer(i4) :: pet_process

    self%n_lai_periods = 1_i4
    self%land_cover%n_periods = 1_i4
    self%active_lai_idx = 0_i4
    self%land_cover%active_idx = 0_i4

    call self%init_land_cover_cache()
    call self%init_land_cover_fraction_cache()

    pet_process = self%exchange%parameters%process_matrix(5, 1)
    need_lai_cache = self%exchange%parameters%process_matrix(1, 1) /= 0_i4 .or. &
      any(pet_process == [-1_i4, 2_i4, 3_i4])
    if (need_lai_cache) then
      if (.not.associated(self%exchange%gridded_lai%data)) then
        log_fatal(*) "MPR: gridded_lai data must be connected before temporal cache initialization."
        error stop 1
      end if
      call self%build_lai_l0_cache()
    end if

    if (self%exchange%parameters%process_matrix(10, 1) > 0_i4 .and. self%exchange%parameters%process_matrix(3, 1) == 0_i4) then
      log_fatal(*) "MPR: neutron regionalization requires an active soil-moisture process."
      error stop 1
    end if

    if (self%exchange%parameters%process_matrix(1, 1) /= 0_i4) call self%init_max_interception_cache()
    if (self%exchange%parameters%process_matrix(2, 1) /= 0_i4) call self%init_snow_cache()
    if (pet_process /= 0_i4) call self%init_pet_cache()
    if (self%exchange%parameters%process_matrix(3, 1) /= 0_i4) call self%init_soil_cache()
    need_runoff_cache = self%exchange%parameters%process_matrix(4, 1) /= 0_i4 .or. &
      self%exchange%parameters%process_matrix(6, 1) /= 0_i4 .or. &
      self%exchange%parameters%process_matrix(7, 1) /= 0_i4 .or. &
      self%exchange%parameters%process_matrix(9, 1) /= 0_i4
    if (need_runoff_cache) then
      if (self%exchange%parameters%process_matrix(3, 1) == 0_i4) then
        log_fatal(*) "MPR: runoff/baseflow parameter generation requires the soil-moisture process to be active."
        error stop 1
      end if
      call self%init_runoff_cache()
    end if
    log_info(*) "MPR: temporal cache initialized (nLAI=", n2s(self%n_lai_periods), &
      ", nLC=", n2s(self%land_cover%n_periods), ")."
  end subroutine mpr_init_temporal_cache

  !> \brief Initialize only the land-cover timing state without caching land-cover input fields.
  subroutine mpr_init_land_cover_timing(self)
    class(mpr_t), intent(inout), target :: self
    type(var) :: land_cover_meta
    integer(i4) :: n_times
    integer(i4) :: i

    if (.not.allocated(self%land_cover%path)) then
      log_fatal(*) "MPR: internal error, land_cover_path not resolved in configure."
      error stop 1
    end if
    if (.not.allocated(self%land_cover%var_name)) then
      log_fatal(*) "MPR: internal error, land_cover_var not resolved in configure."
      error stop 1
    end if
    if (.not.associated(self%exchange%level0)) then
      log_fatal(*) "MPR: level0 grid not connected before land-cover timing initialization."
      error stop 1
    end if

    if (allocated(self%land_cover%ds%vars)) call self%land_cover%ds%close()
    if (allocated(self%land_cover%period_start)) deallocate(self%land_cover%period_start)
    if (allocated(self%land_cover%period_end)) deallocate(self%land_cover%period_end)
    self%land_cover%temporal = .false.
    self%land_cover%n_periods = 1_i4

    call self%land_cover%ds%init( &
      path=self%land_cover%path, &
      vars=[var(name=trim(self%land_cover%var_name), kind="i4", static=.false., allow_static=.true.)], &
      grid=self%exchange%level0, &
      timestamp=start_timestamp)

    land_cover_meta = self%land_cover%ds%meta(trim(self%land_cover%var_name))
    if (land_cover_meta%static) then
      self%land_cover%temporal = .false.
      self%land_cover%n_periods = 1_i4
      call self%land_cover%ds%close()
      return
    end if

    if (self%exchange%start_time < self%land_cover%ds%start_time) then
      log_fatal(*) "MPR: temporal land-cover coverage starts at ", self%land_cover%ds%start_time%str(), &
        ", but simulation starts at ", self%exchange%start_time%str(), "."
      error stop 1
    end if
    n_times = size(self%land_cover%ds%times)
    if (n_times < 1_i4) then
      log_fatal(*) "MPR: temporal land-cover dataset contains no time steps."
      error stop 1
    end if
    if (self%exchange%end_time > self%land_cover%ds%times(n_times)) then
      log_fatal(*) "MPR: temporal land-cover coverage ends at ", self%land_cover%ds%times(n_times)%str(), &
        ", but simulation ends at ", self%exchange%end_time%str(), "."
      error stop 1
    end if

    self%land_cover%n_periods = n_times
    allocate(self%land_cover%period_start(self%land_cover%n_periods))
    allocate(self%land_cover%period_end(self%land_cover%n_periods))
    self%land_cover%period_start(1) = self%land_cover%ds%start_time
    self%land_cover%period_end = self%land_cover%ds%times
    do i = 2_i4, self%land_cover%n_periods
      self%land_cover%period_start(i) = self%land_cover%ds%times(i - 1_i4)
    end do

    self%land_cover%temporal = .true.
    call self%land_cover%ds%close()
  end subroutine mpr_init_land_cover_timing

  !> \brief Build cached land-cover fields on level0 for static or temporal datasets.
  subroutine mpr_init_land_cover_cache(self)
    class(mpr_t), intent(inout), target :: self

    if (allocated(self%land_cover%l0_cache)) deallocate(self%land_cover%l0_cache)
    call self%init_land_cover_timing()

    call self%land_cover%ds%init( &
      path=self%land_cover%path, &
      vars=[var(name=trim(self%land_cover%var_name), kind="i4", static=.false., allow_static=.true.)], &
      grid=self%exchange%level0, &
      timestamp=start_timestamp)
    if (.not.self%land_cover%temporal) then
      allocate(self%land_cover%l0_cache(self%exchange%level0%nCells, 1))
      call self%land_cover%ds%read(trim(self%land_cover%var_name), self%land_cover%l0_cache(:, 1))
      call self%land_cover%ds%close()
      log_info(*) "MPR: static land-cover dataset initialized (nLC=1)."
      return
    end if

    call self%land_cover%ds%read_chunk( &
      trim(self%land_cover%var_name), self%land_cover%l0_cache, &
      self%land_cover%period_start(1), self%land_cover%period_end(self%land_cover%n_periods))
    call self%land_cover%ds%close()
    log_info(*) "MPR: temporal land-cover dataset initialized (nLC=", n2s(self%land_cover%n_periods), ")."
  end subroutine mpr_init_land_cover_cache

  !> \brief Upscale cached land-cover scenes to level1 area fractions for downstream MPR groups.
  subroutine mpr_init_land_cover_fraction_cache(self)
    class(mpr_t), intent(inout), target :: self
    integer(i4) :: land_cover_idx
    integer(i4) :: id(1)
    real(dp) :: frac_sealed_cityarea

    if (.not.allocated(self%land_cover%l0_cache)) then
      log_fatal(*) "MPR: land-cover level0 cache not available before land-cover fraction initialization."
      error stop 1
    end if

    id(1) = self%exchange%domain
    frac_sealed_cityarea = self%config%fracsealed_cityarea(id(1))
    if (.not.ieee_is_finite(frac_sealed_cityarea)) frac_sealed_cityarea = 0.0_dp

    if (allocated(self%land_cover%forest_fraction_l1)) deallocate(self%land_cover%forest_fraction_l1)
    if (allocated(self%land_cover%sealed_fraction_l1)) deallocate(self%land_cover%sealed_fraction_l1)
    if (allocated(self%land_cover%pervious_fraction_l1)) deallocate(self%land_cover%pervious_fraction_l1)
    allocate(self%land_cover%forest_fraction_l1(self%exchange%level1%ncells, self%land_cover%n_periods))
    allocate(self%land_cover%sealed_fraction_l1(self%exchange%level1%ncells, self%land_cover%n_periods))
    allocate(self%land_cover%pervious_fraction_l1(self%exchange%level1%ncells, self%land_cover%n_periods))

    do land_cover_idx = 1_i4, self%land_cover%n_periods
      call mpr_bridge_land_cover_fraction( &
        self%upscaler, self%land_cover%l0_cache(:, land_cover_idx), frac_sealed_cityarea, &
        self%land_cover%forest_fraction_l1(:, land_cover_idx), &
        self%land_cover%sealed_fraction_l1(:, land_cover_idx), &
        self%land_cover%pervious_fraction_l1(:, land_cover_idx))
    end do

    self%exchange%f_sealed%provided = .true.
  end subroutine mpr_init_land_cover_fraction_cache

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
        if (size(lai_lut, 2) < YEAR_MONTHS) then
          log_fatal(*) "MPR: LAI LUT provides ", n2s(size(lai_lut, 2)), " periods, expected at least ", n2s(YEAR_MONTHS), "."
          error stop 1
        end if

        self%n_lai_periods = YEAR_MONTHS
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

  !> \brief Load one configured process parameter block into an allocated local array.
  subroutine mpr_load_process_params(self, process_id, params)
    class(mpr_t), intent(in), target :: self
    integer(i4), intent(in) :: process_id
    real(dp), allocatable, intent(out) :: params(:)
    integer(i4) :: n_param

    n_param = self%exchange%parameters%process_matrix(process_id, 2)
    if (n_param < 0_i4) then
      log_fatal(*) "MPR: configured parameter count for process ", n2s(process_id), " must be >= 0."
      error stop 1
    end if
    if (n_param == 0_i4) then
      allocate(params(0))
      return
    end if

    allocate(params(n_param))
    params = self%exchange%parameters%get_process(process_id)
  end subroutine mpr_load_process_params

  !> \brief Cache max interception on level1 for all LAI/land-cover slices.
  subroutine mpr_init_max_interception_cache(self)
    class(mpr_t), intent(inout), target :: self
    integer(i4) :: land_cover_idx
    integer(i4) :: lai_idx
    real(dp), allocatable :: interception_param(:)
    real(dp), allocatable :: max_interception_l0(:)
    real(dp), allocatable :: max_interception_l1(:)

    call self%load_process_params(1_i4, interception_param)
    if (size(interception_param) < 1_i4) then
      log_fatal(*) "MPR: interception parameter set is empty while process 1 is active."
      error stop 1
    end if
    if (.not.allocated(self%lai_l0_cache)) then
      log_fatal(*) "MPR: LAI level0 cache not available before max interception cache initialization."
      error stop 1
    end if
    if (.not.allocated(self%land_cover%l0_cache)) then
      log_fatal(*) "MPR: land-cover level0 cache not available before max interception cache initialization."
      error stop 1
    end if
    if (size(self%land_cover%l0_cache, 2) /= self%land_cover%n_periods) then
      log_fatal(*) "MPR: land-cover cache period count does not match n_land_cover_periods."
      error stop 1
    end if

    if (allocated(self%max_interception_cache)) deallocate(self%max_interception_cache)
    allocate(self%max_interception_cache(self%exchange%level1%ncells, self%n_lai_periods, self%land_cover%n_periods))
    allocate(max_interception_l0(self%exchange%level0%ncells))
    allocate(max_interception_l1(self%exchange%level1%ncells))
    do lai_idx = 1_i4, self%n_lai_periods
      max_interception_l0 = interception_param(1) * self%lai_l0_cache(:, lai_idx)
      call self%upscaler%execute(max_interception_l0, max_interception_l1, upscaling_operator=up_a_mean)
      do land_cover_idx = 1_i4, self%land_cover%n_periods
        self%max_interception_cache(:, lai_idx, land_cover_idx) = max_interception_l1
      end do
    end do
    self%exchange%max_interception%provided = .true.
  end subroutine mpr_init_max_interception_cache

  !> \brief Cache snow parameter fields on level1 for all land-cover slices.
  subroutine mpr_init_snow_cache(self)
    class(mpr_t), intent(inout), target :: self
    integer(i4) :: land_cover_idx
    real(dp), allocatable :: snow_param(:)

    call self%load_process_params(2_i4, snow_param)
    if (size(snow_param) < 8_i4) then
      log_fatal(*) "MPR: snow parameter set must contain 8 values while process 2 is active."
      error stop 1
    end if
    if (.not.allocated(self%land_cover%forest_fraction_l1)) then
      log_fatal(*) "MPR: land-cover fractions not available before snow cache initialization."
      error stop 1
    end if

    if (allocated(self%snow%thresh_temp_cache)) deallocate(self%snow%thresh_temp_cache)
    if (allocated(self%snow%degday_dry_cache)) deallocate(self%snow%degday_dry_cache)
    if (allocated(self%snow%degday_inc_cache)) deallocate(self%snow%degday_inc_cache)
    if (allocated(self%snow%degday_max_cache)) deallocate(self%snow%degday_max_cache)
    allocate(self%snow%thresh_temp_cache(self%exchange%level1%ncells, self%land_cover%n_periods))
    allocate(self%snow%degday_dry_cache(self%exchange%level1%ncells, self%land_cover%n_periods))
    allocate(self%snow%degday_inc_cache(self%exchange%level1%ncells, self%land_cover%n_periods))
    allocate(self%snow%degday_max_cache(self%exchange%level1%ncells, self%land_cover%n_periods))

    do land_cover_idx = 1_i4, self%land_cover%n_periods
      call mpr_bridge_snow_param( &
        snow_param, &
        self%land_cover%forest_fraction_l1(:, land_cover_idx), &
        self%land_cover%sealed_fraction_l1(:, land_cover_idx), &
        self%land_cover%pervious_fraction_l1(:, land_cover_idx), &
        self%snow%thresh_temp_cache(:, land_cover_idx), &
        self%snow%degday_dry_cache(:, land_cover_idx), &
        self%snow%degday_inc_cache(:, land_cover_idx), &
        self%snow%degday_max_cache(:, land_cover_idx))
    end do

    self%exchange%thresh_temp%provided = .true.
    self%exchange%degday_dry%provided = .true.
    self%exchange%degday_inc%provided = .true.
    self%exchange%degday_max%provided = .true.
  end subroutine mpr_init_snow_cache

  !> \brief Cache PET parameter fields on level1 for the active PET process.
  subroutine mpr_init_pet_cache(self)
    class(mpr_t), intent(inout), target :: self
    integer(i4) :: pet_process

    pet_process = self%exchange%parameters%process_matrix(5, 1)
    select case (pet_process)
      case (-2_i4)
        call self%init_pet_aspect_cache()
      case (-1_i4)
        call self%init_pet_lai_cache()
      case (1_i4)
        call self%init_pet_hargreaves_cache()
      case (2_i4)
        call self%init_pet_priestley_taylor_cache()
      case (3_i4)
        call self%init_pet_penman_cache()
      case default
        log_fatal(*) "MPR: unsupported PET process case ", n2s(pet_process), "."
        error stop 1
    end select
  end subroutine mpr_init_pet_cache

  !> \brief Cache static PET aspect correction on level1.
  subroutine mpr_init_pet_aspect_cache(self)
    class(mpr_t), intent(inout), target :: self
    real(dp), allocatable :: pet_param(:)

    call self%load_process_params(5_i4, pet_param)
    if (size(pet_param) < 3_i4) then
      log_fatal(*) "MPR: PET aspect parameter set must contain 3 values while PET process -2 is active."
      error stop 1
    end if
    if (.not.associated(self%exchange%aspect%data)) then
      log_fatal(*) "MPR: aspect data must be connected before PET aspect cache initialization."
      error stop 1
    end if

    if (allocated(self%pet%pet_fac_aspect_cache)) deallocate(self%pet%pet_fac_aspect_cache)
    allocate(self%pet%pet_fac_aspect_cache(self%exchange%level1%ncells))
    call mpr_bridge_pet_aspect(self%exchange%level0, self%exchange%aspect%data, pet_param, self%upscaler, &
      self%pet%pet_fac_aspect_cache)

    self%exchange%pet_fac_aspect%provided = .true.
  end subroutine mpr_init_pet_aspect_cache

  !> \brief Cache Hargreaves-Samani PET coefficients on level1.
  subroutine mpr_init_pet_hargreaves_cache(self)
    class(mpr_t), intent(inout), target :: self
    real(dp), allocatable :: pet_param(:)

    call self%load_process_params(5_i4, pet_param)
    if (size(pet_param) < 4_i4) then
      log_fatal(*) "MPR: PET Hargreaves parameter set must contain 4 values while PET process 1 is active."
      error stop 1
    end if
    if (.not.associated(self%exchange%aspect%data)) then
      log_fatal(*) "MPR: aspect data must be connected before PET Hargreaves cache initialization."
      error stop 1
    end if

    if (allocated(self%pet%pet_fac_aspect_cache)) deallocate(self%pet%pet_fac_aspect_cache)
    if (allocated(self%pet%pet_coeff_hs_cache)) deallocate(self%pet%pet_coeff_hs_cache)
    allocate(self%pet%pet_fac_aspect_cache(self%exchange%level1%ncells))
    allocate(self%pet%pet_coeff_hs_cache(self%exchange%level1%ncells))
    call mpr_bridge_pet_hargreaves(self%exchange%level0, self%exchange%aspect%data, pet_param, self%upscaler, &
      self%pet%pet_fac_aspect_cache, self%pet%pet_coeff_hs_cache)

    self%exchange%pet_fac_aspect%provided = .true.
    self%exchange%pet_coeff_hs%provided = .true.
  end subroutine mpr_init_pet_hargreaves_cache

  !> \brief Cache PET LAI correction on level1 for all LAI/land-cover slices.
  subroutine mpr_init_pet_lai_cache(self)
    class(mpr_t), intent(inout), target :: self
    integer(i4) :: land_cover_idx
    integer(i4) :: lai_idx
    real(dp), allocatable :: pet_param(:)

    call self%load_process_params(5_i4, pet_param)
    if (size(pet_param) < 5_i4) then
      log_fatal(*) "MPR: PET-LAI parameter set must contain 5 values while PET process -1 is active."
      error stop 1
    end if
    if (.not.allocated(self%lai_l0_cache)) then
      log_fatal(*) "MPR: LAI level0 cache not available before PET-LAI cache initialization."
      error stop 1
    end if
    if (.not.allocated(self%land_cover%l0_cache)) then
      log_fatal(*) "MPR: land-cover level0 cache not available before PET-LAI cache initialization."
      error stop 1
    end if

    if (allocated(self%pet%pet_fac_lai_cache)) deallocate(self%pet%pet_fac_lai_cache)
    allocate(self%pet%pet_fac_lai_cache(self%exchange%level1%ncells, self%n_lai_periods, self%land_cover%n_periods))
    do land_cover_idx = 1_i4, self%land_cover%n_periods
      do lai_idx = 1_i4, self%n_lai_periods
        call mpr_bridge_pet_lai( &
          pet_param, self%land_cover%l0_cache(:, land_cover_idx), self%lai_l0_cache(:, lai_idx), &
          self%upscaler, self%pet%pet_fac_lai_cache(:, lai_idx, land_cover_idx))
      end do
    end do

    self%exchange%pet_fac_lai%provided = .true.
  end subroutine mpr_init_pet_lai_cache

  !> \brief Cache Priestley-Taylor PET coefficients on level1 for all LAI slices.
  subroutine mpr_init_pet_priestley_taylor_cache(self)
    class(mpr_t), intent(inout), target :: self
    real(dp), allocatable :: pet_param(:)

    call self%load_process_params(5_i4, pet_param)
    if (size(pet_param) < 2_i4) then
      log_fatal(*) "MPR: PET Priestley-Taylor parameter set must contain 2 values while PET process 2 is active."
      error stop 1
    end if
    if (.not.allocated(self%lai_l0_cache)) then
      log_fatal(*) "MPR: LAI level0 cache not available before PET Priestley-Taylor cache initialization."
      error stop 1
    end if

    if (allocated(self%pet%pet_coeff_pt_cache)) deallocate(self%pet%pet_coeff_pt_cache)
    allocate(self%pet%pet_coeff_pt_cache(self%exchange%level1%ncells, self%n_lai_periods))
    call mpr_bridge_pet_priestley_taylor(self%exchange%level0, self%lai_l0_cache, pet_param, self%upscaler, &
      self%pet%pet_coeff_pt_cache)

    self%exchange%pet_coeff_pt%provided = .true.
  end subroutine mpr_init_pet_priestley_taylor_cache

  !> \brief Cache Penman-Monteith PET resistance fields on level1 for all LAI/land-cover slices.
  subroutine mpr_init_pet_penman_cache(self)
    class(mpr_t), intent(inout), target :: self
    integer(i4) :: land_cover_idx
    real(dp), allocatable :: pet_param(:)
    real(dp), allocatable :: resist_surf_l1(:, :)

    call self%load_process_params(5_i4, pet_param)
    if (size(pet_param) < 7_i4) then
      log_fatal(*) "MPR: PET Penman-Monteith parameter set must contain 7 values while PET process 3 is active."
      error stop 1
    end if
    if (.not.allocated(self%lai_l0_cache)) then
      log_fatal(*) "MPR: LAI level0 cache not available before PET Penman-Monteith cache initialization."
      error stop 1
    end if
    if (.not.allocated(self%land_cover%l0_cache)) then
      log_fatal(*) "MPR: land-cover level0 cache not available before PET Penman-Monteith cache initialization."
      error stop 1
    end if

    if (allocated(self%pet%resist_aero_cache)) deallocate(self%pet%resist_aero_cache)
    if (allocated(self%pet%resist_surf_cache)) deallocate(self%pet%resist_surf_cache)
    allocate(self%pet%resist_aero_cache(self%exchange%level1%ncells, self%n_lai_periods, self%land_cover%n_periods))
    allocate(self%pet%resist_surf_cache(self%exchange%level1%ncells, self%n_lai_periods))
    allocate(resist_surf_l1(self%exchange%level1%ncells, self%n_lai_periods))

    do land_cover_idx = 1_i4, self%land_cover%n_periods
      call mpr_bridge_pet_penman_monteith(self%exchange%level0, self%land_cover%l0_cache(:, land_cover_idx), &
        self%lai_l0_cache, pet_param, self%upscaler, self%pet%resist_aero_cache(:, :, land_cover_idx), resist_surf_l1)
      if (land_cover_idx == 1_i4) self%pet%resist_surf_cache = resist_surf_l1
    end do
    deallocate(resist_surf_l1)

    self%exchange%resist_aero%provided = .true.
    self%exchange%resist_surf%provided = .true.
  end subroutine mpr_init_pet_penman_cache

  !> \brief Cache soil-moisture parameter fields on level1 for all land-cover slices.
  subroutine mpr_init_soil_cache(self)
    class(mpr_t), intent(inout), target :: self
    integer(i4) :: domain_id
    integer(i4) :: n_horizons
    integer(i4) :: neutron_process
    integer(i4) :: n_soil_layers
    integer(i4) :: land_cover_idx
    real(dp), allocatable :: neutron_param(:)
    real(dp), allocatable :: soil_param(:)

    domain_id = self%exchange%domain
    n_horizons = self%config%n_horizons(domain_id)
    neutron_process = self%exchange%parameters%process_matrix(10, 1)
    if (n_horizons < 1_i4) then
      log_fatal(*) "MPR: n_horizons must be >= 1 before soil cache initialization."
      error stop 1
    end if
    if (.not.allocated(self%soil_lut_path)) then
      log_fatal(*) "MPR: internal error, soil_lut_path not resolved in configure."
      error stop 1
    end if
    if (.not.allocated(self%land_cover%l0_cache)) then
      log_fatal(*) "MPR: land-cover level0 cache not available before soil cache initialization."
      error stop 1
    end if
    if (.not.associated(self%exchange%soil_id%data)) then
      log_fatal(*) "MPR: soil_id data not connected before soil cache initialization."
      error stop 1
    end if

    n_soil_layers = 1_i4
    if (self%config%soil_db_mode(domain_id) == 1_i4) n_soil_layers = n_horizons
    if (allocated(self%soil%horizon_bounds)) deallocate(self%soil%horizon_bounds)
    call mpr_bridge_setup_soil_database( &
      self%config, domain_id, self%soil_lut_path, self%exchange%soil_id%data(:, :n_soil_layers), &
      self%soil%horizon_bounds)

    call self%load_process_params(3_i4, soil_param)
    if (allocated(self%soil%sm_exponent_cache)) deallocate(self%soil%sm_exponent_cache)
    if (allocated(self%soil%sm_saturation_cache)) deallocate(self%soil%sm_saturation_cache)
    if (allocated(self%soil%sm_field_capacity_cache)) deallocate(self%soil%sm_field_capacity_cache)
    if (allocated(self%soil%wilting_point_cache)) deallocate(self%soil%wilting_point_cache)
    if (allocated(self%soil%f_roots_cache)) deallocate(self%soil%f_roots_cache)
    if (allocated(self%soil%thresh_jarvis_cache)) deallocate(self%soil%thresh_jarvis_cache)
    if (allocated(self%soil%sm_deficit_fc_l0)) deallocate(self%soil%sm_deficit_fc_l0)
    if (allocated(self%soil%ks_var_h_l0)) deallocate(self%soil%ks_var_h_l0)
    if (allocated(self%soil%ks_var_v_l0)) deallocate(self%soil%ks_var_v_l0)
    if (allocated(self%neutron%desilets_n0_cache)) deallocate(self%neutron%desilets_n0_cache)
    if (allocated(self%neutron%bulk_density_cache)) deallocate(self%neutron%bulk_density_cache)
    if (allocated(self%neutron%lattice_water_cache)) deallocate(self%neutron%lattice_water_cache)
    if (allocated(self%neutron%cosmic_l3_cache)) deallocate(self%neutron%cosmic_l3_cache)
    allocate(self%soil%sm_exponent_cache(self%exchange%level1%ncells, n_horizons, self%land_cover%n_periods))
    allocate(self%soil%sm_saturation_cache(self%exchange%level1%ncells, n_horizons, self%land_cover%n_periods))
    allocate(self%soil%sm_field_capacity_cache(self%exchange%level1%ncells, n_horizons, self%land_cover%n_periods))
    allocate(self%soil%wilting_point_cache(self%exchange%level1%ncells, n_horizons, self%land_cover%n_periods))
    allocate(self%soil%f_roots_cache(self%exchange%level1%ncells, n_horizons, self%land_cover%n_periods))
    allocate(self%soil%thresh_jarvis_cache(self%exchange%level1%ncells))
    allocate(self%soil%sm_deficit_fc_l0(self%exchange%level0%ncells, self%land_cover%n_periods))
    allocate(self%soil%ks_var_h_l0(self%exchange%level0%ncells, self%land_cover%n_periods))
    allocate(self%soil%ks_var_v_l0(self%exchange%level0%ncells, self%land_cover%n_periods))
    if (neutron_process > 0_i4) then
      call self%load_process_params(10_i4, neutron_param)
      if (size(neutron_param) < 1_i4) then
        log_fatal(*) "MPR: neutron process definition is empty."
        error stop 1
      end if
      allocate(self%neutron%desilets_n0_cache(self%exchange%level1%ncells))
      allocate(self%neutron%bulk_density_cache(self%exchange%level1%ncells, n_horizons, self%land_cover%n_periods))
      allocate(self%neutron%lattice_water_cache(self%exchange%level1%ncells, n_horizons, self%land_cover%n_periods))
      self%neutron%desilets_n0_cache = neutron_param(1)
      if (neutron_process == 2_i4) then
        allocate(self%neutron%cosmic_l3_cache(self%exchange%level1%ncells, n_horizons, self%land_cover%n_periods))
      end if
    end if

    do land_cover_idx = 1_i4, self%land_cover%n_periods
      if (neutron_process == 2_i4) then
        call mpr_bridge_soil_moisture( &
          self%exchange%parameters%process_matrix, soil_param, self%land_cover%l0_cache(:, land_cover_idx), &
          self%exchange%soil_id%data(:, :n_soil_layers), self%exchange%level0, self%upscaler, &
          self%soil%thresh_jarvis_cache, self%soil%sm_exponent_cache(:, :, land_cover_idx), &
          self%soil%sm_saturation_cache(:, :, land_cover_idx), self%soil%sm_field_capacity_cache(:, :, land_cover_idx), &
          self%soil%wilting_point_cache(:, :, land_cover_idx), self%soil%f_roots_cache(:, :, land_cover_idx), &
          self%soil%sm_deficit_fc_l0(:, land_cover_idx), self%soil%ks_var_h_l0(:, land_cover_idx), &
          self%soil%ks_var_v_l0(:, land_cover_idx), self%neutron%bulk_density_cache(:, :, land_cover_idx), &
          self%neutron%lattice_water_cache(:, :, land_cover_idx), self%neutron%cosmic_l3_cache(:, :, land_cover_idx), &
          neutron_param)
      else if (neutron_process == 1_i4) then
        call mpr_bridge_soil_moisture( &
          self%exchange%parameters%process_matrix, soil_param, self%land_cover%l0_cache(:, land_cover_idx), &
          self%exchange%soil_id%data(:, :n_soil_layers), self%exchange%level0, self%upscaler, &
          self%soil%thresh_jarvis_cache, self%soil%sm_exponent_cache(:, :, land_cover_idx), &
          self%soil%sm_saturation_cache(:, :, land_cover_idx), self%soil%sm_field_capacity_cache(:, :, land_cover_idx), &
          self%soil%wilting_point_cache(:, :, land_cover_idx), self%soil%f_roots_cache(:, :, land_cover_idx), &
          self%soil%sm_deficit_fc_l0(:, land_cover_idx), self%soil%ks_var_h_l0(:, land_cover_idx), &
          self%soil%ks_var_v_l0(:, land_cover_idx), self%neutron%bulk_density_cache(:, :, land_cover_idx), &
          self%neutron%lattice_water_cache(:, :, land_cover_idx), neutron_param=neutron_param)
      else
        call mpr_bridge_soil_moisture( &
          self%exchange%parameters%process_matrix, soil_param, self%land_cover%l0_cache(:, land_cover_idx), &
          self%exchange%soil_id%data(:, :n_soil_layers), self%exchange%level0, self%upscaler, &
          self%soil%thresh_jarvis_cache, self%soil%sm_exponent_cache(:, :, land_cover_idx), &
          self%soil%sm_saturation_cache(:, :, land_cover_idx), self%soil%sm_field_capacity_cache(:, :, land_cover_idx), &
          self%soil%wilting_point_cache(:, :, land_cover_idx), self%soil%f_roots_cache(:, :, land_cover_idx), &
          self%soil%sm_deficit_fc_l0(:, land_cover_idx), self%soil%ks_var_h_l0(:, land_cover_idx), &
          self%soil%ks_var_v_l0(:, land_cover_idx))
      end if
    end do

    self%exchange%f_roots%provided = .true.
    self%exchange%sm_saturation%provided = .true.
    self%exchange%sm_exponent%provided = .true.
    self%exchange%sm_field_capacity%provided = .true.
    self%exchange%wilting_point%provided = .true.
    self%exchange%thresh_jarvis%provided = .true.
    if (allocated(self%neutron%desilets_n0_cache)) self%exchange%desilets_n0%provided = .true.
    if (allocated(self%neutron%bulk_density_cache)) self%exchange%bulk_density%provided = .true.
    if (allocated(self%neutron%lattice_water_cache)) self%exchange%lattice_water%provided = .true.
    if (allocated(self%neutron%cosmic_l3_cache)) self%exchange%cosmic_l3%provided = .true.
  end subroutine mpr_init_soil_cache

  !> \brief Cache runoff and baseflow parameter fields on level1.
  subroutine mpr_init_runoff_cache(self)
    class(mpr_t), intent(inout), target :: self
    integer(i4) :: land_cover_idx
    real(dp), allocatable :: direct_runoff_param(:)
    real(dp), allocatable :: interflow_param(:)
    real(dp), allocatable :: percolation_param(:)
    real(dp), allocatable :: baseflow_param(:)

    if (.not.allocated(self%soil%sm_deficit_fc_l0) .or. .not.allocated(self%soil%ks_var_h_l0) .or. &
      .not.allocated(self%soil%ks_var_v_l0)) then
      log_fatal(*) "MPR: soil L0 intermediates are not available before runoff/baseflow cache initialization."
      error stop 1
    end if

    if (allocated(self%runoff%alpha_cache)) deallocate(self%runoff%alpha_cache)
    if (allocated(self%runoff%k_fastflow_cache)) deallocate(self%runoff%k_fastflow_cache)
    if (allocated(self%runoff%k_slowflow_cache)) deallocate(self%runoff%k_slowflow_cache)
    if (allocated(self%runoff%k_baseflow_cache)) deallocate(self%runoff%k_baseflow_cache)
    if (allocated(self%runoff%k_percolation_cache)) deallocate(self%runoff%k_percolation_cache)
    if (allocated(self%runoff%f_karst_loss_cache)) deallocate(self%runoff%f_karst_loss_cache)
    if (allocated(self%runoff%thresh_unsat_cache)) deallocate(self%runoff%thresh_unsat_cache)
    if (allocated(self%runoff%thresh_sealed_cache)) deallocate(self%runoff%thresh_sealed_cache)

    if (self%exchange%parameters%process_matrix(6, 1) /= 0_i4) then
      allocate(self%runoff%alpha_cache(self%exchange%level1%ncells, self%land_cover%n_periods))
      allocate(self%runoff%k_fastflow_cache(self%exchange%level1%ncells, self%land_cover%n_periods))
      allocate(self%runoff%k_slowflow_cache(self%exchange%level1%ncells, self%land_cover%n_periods))
      allocate(self%runoff%thresh_unsat_cache(self%exchange%level1%ncells))
      call self%load_process_params(6_i4, interflow_param)
      do land_cover_idx = 1_i4, self%land_cover%n_periods
        call mpr_bridge_runoff_param( &
          self%land_cover%l0_cache(:, land_cover_idx), self%slope_emp, self%soil%sm_deficit_fc_l0(:, land_cover_idx), &
          self%soil%ks_var_h_l0(:, land_cover_idx), self%exchange%level0, self%upscaler, interflow_param, &
          self%runoff%thresh_unsat_cache, self%runoff%k_fastflow_cache(:, land_cover_idx), &
          self%runoff%k_slowflow_cache(:, land_cover_idx), self%runoff%alpha_cache(:, land_cover_idx))
      end do
      self%exchange%alpha%provided = .true.
      self%exchange%k_fastflow%provided = .true.
      self%exchange%k_slowflow%provided = .true.
      self%exchange%thresh_unsat%provided = .true.
    end if

    if (self%exchange%parameters%process_matrix(7, 1) /= 0_i4) then
      allocate(self%runoff%k_percolation_cache(self%exchange%level1%ncells, self%land_cover%n_periods))
      allocate(self%runoff%f_karst_loss_cache(self%exchange%level1%ncells))
      call self%load_process_params(7_i4, percolation_param)
      do land_cover_idx = 1_i4, self%land_cover%n_periods
        call mpr_bridge_karstic_param( &
          percolation_param, self%exchange%geo_unit%data, self%exchange%geo_class_def%geo_unit, &
          self%exchange%geo_class_def%geo_karstic, self%soil%sm_deficit_fc_l0(:, land_cover_idx), &
          self%soil%ks_var_v_l0(:, land_cover_idx), self%upscaler, self%runoff%f_karst_loss_cache, &
          self%runoff%k_percolation_cache(:, land_cover_idx))
      end do
      self%exchange%k_percolation%provided = .true.
      self%exchange%f_karst_loss%provided = .true.
    end if

    if (self%exchange%parameters%process_matrix(4, 1) /= 0_i4) then
      allocate(self%runoff%thresh_sealed_cache(self%exchange%level1%ncells))
      call self%load_process_params(4_i4, direct_runoff_param)
      call mpr_bridge_sealed_threshold(direct_runoff_param, self%runoff%thresh_sealed_cache)
      self%exchange%thresh_sealed%provided = .true.
    end if

    if (self%exchange%parameters%process_matrix(9, 1) /= 0_i4) then
      allocate(self%runoff%k_baseflow_cache(self%exchange%level1%ncells, self%land_cover%n_periods))
      call self%load_process_params(9_i4, baseflow_param)
      call mpr_bridge_baseflow_param( &
        baseflow_param, self%exchange%geo_unit%data, self%exchange%geo_class_def%geo_unit, self%upscaler, &
        self%runoff%k_baseflow_cache(:, 1))
      do land_cover_idx = 2_i4, self%land_cover%n_periods
        self%runoff%k_baseflow_cache(:, land_cover_idx) = self%runoff%k_baseflow_cache(:, 1)
      end do
      if (allocated(self%runoff%k_slowflow_cache) .and. self%exchange%parameters%process_matrix(7, 1) > 0_i4) then
        self%runoff%k_baseflow_cache = merge(self%runoff%k_slowflow_cache, self%runoff%k_baseflow_cache, &
          self%runoff%k_baseflow_cache < self%runoff%k_slowflow_cache)
      end if
      self%exchange%k_baseflow%provided = .true.
    end if
  end subroutine mpr_init_runoff_cache

  !> \brief Switch exchange pointers to active cached MPR slices for current model time.
  subroutine mpr_update_exchange_slices(self, force)
    class(mpr_t), intent(inout), target :: self
    logical, optional, intent(in) :: force
    logical :: force_
    integer(i4) :: lai_idx
    integer(i4) :: land_cover_idx

    force_ = optval(force, .false.)

    if (.not.allocated(self%land_cover%sealed_fraction_l1) .and. &
      .not.allocated(self%max_interception_cache) .and. &
      .not.allocated(self%snow%thresh_temp_cache) .and. &
      .not.allocated(self%pet%pet_fac_aspect_cache) .and. &
      .not.allocated(self%pet%pet_coeff_hs_cache) .and. &
      .not.allocated(self%pet%pet_coeff_pt_cache) .and. &
      .not.allocated(self%pet%pet_fac_lai_cache) .and. &
      .not.allocated(self%pet%resist_aero_cache) .and. &
      .not.allocated(self%pet%resist_surf_cache) .and. &
      .not.allocated(self%soil%sm_exponent_cache) .and. &
      .not.allocated(self%soil%thresh_jarvis_cache) .and. &
      .not.allocated(self%neutron%desilets_n0_cache) .and. &
      .not.allocated(self%neutron%bulk_density_cache) .and. &
      .not.allocated(self%neutron%lattice_water_cache) .and. &
      .not.allocated(self%neutron%cosmic_l3_cache) .and. &
      .not.allocated(self%runoff%alpha_cache) .and. &
      .not.allocated(self%runoff%f_karst_loss_cache) .and. &
      .not.allocated(self%runoff%thresh_sealed_cache)) return

    lai_idx = self%lai_index_for_time()
    land_cover_idx = self%land_cover_index_for_time()
    if (lai_idx < 1_i4 .or. lai_idx > self%n_lai_periods) then
      log_fatal(*) "MPR: LAI index out of bounds: ", n2s(lai_idx), " not in [1,", n2s(self%n_lai_periods), "]."
      error stop 1
    end if
    if (land_cover_idx < 1_i4 .or. land_cover_idx > self%land_cover%n_periods) then
      log_fatal(*) "MPR: land-cover index out of bounds: ", n2s(land_cover_idx), " not in [1,", n2s(self%land_cover%n_periods), "]."
      error stop 1
    end if
    if (.not.force_ .and. lai_idx == self%active_lai_idx .and. land_cover_idx == self%land_cover%active_idx) return

    if (allocated(self%land_cover%sealed_fraction_l1)) then
      self%exchange%f_sealed%data => self%land_cover%sealed_fraction_l1(:, land_cover_idx)
    end if
    if (allocated(self%max_interception_cache)) then
      self%exchange%max_interception%data => self%max_interception_cache(:, lai_idx, land_cover_idx)
    end if
    if (allocated(self%snow%thresh_temp_cache)) then
      self%exchange%thresh_temp%data => self%snow%thresh_temp_cache(:, land_cover_idx)
      self%exchange%degday_dry%data => self%snow%degday_dry_cache(:, land_cover_idx)
      self%exchange%degday_inc%data => self%snow%degday_inc_cache(:, land_cover_idx)
      self%exchange%degday_max%data => self%snow%degday_max_cache(:, land_cover_idx)
    end if
    if (allocated(self%pet%pet_fac_aspect_cache)) then
      self%exchange%pet_fac_aspect%data => self%pet%pet_fac_aspect_cache
    end if
    if (allocated(self%pet%pet_coeff_hs_cache)) then
      self%exchange%pet_coeff_hs%data => self%pet%pet_coeff_hs_cache
    end if
    if (allocated(self%pet%pet_coeff_pt_cache)) then
      self%exchange%pet_coeff_pt%data => self%pet%pet_coeff_pt_cache(:, lai_idx)
    end if
    if (allocated(self%pet%pet_fac_lai_cache)) then
      self%exchange%pet_fac_lai%data => self%pet%pet_fac_lai_cache(:, lai_idx, land_cover_idx)
    end if
    if (allocated(self%pet%resist_aero_cache)) then
      self%exchange%resist_aero%data => self%pet%resist_aero_cache(:, lai_idx, land_cover_idx)
    end if
    if (allocated(self%pet%resist_surf_cache)) then
      self%exchange%resist_surf%data => self%pet%resist_surf_cache(:, lai_idx)
    end if
    if (allocated(self%soil%sm_exponent_cache)) then
      self%exchange%f_roots%data => self%soil%f_roots_cache(:, :, land_cover_idx)
      self%exchange%sm_saturation%data => self%soil%sm_saturation_cache(:, :, land_cover_idx)
      self%exchange%sm_exponent%data => self%soil%sm_exponent_cache(:, :, land_cover_idx)
      self%exchange%sm_field_capacity%data => self%soil%sm_field_capacity_cache(:, :, land_cover_idx)
      self%exchange%wilting_point%data => self%soil%wilting_point_cache(:, :, land_cover_idx)
    end if
    if (allocated(self%soil%thresh_jarvis_cache)) then
      self%exchange%thresh_jarvis%data => self%soil%thresh_jarvis_cache
    end if
    if (allocated(self%neutron%desilets_n0_cache)) then
      self%exchange%desilets_n0%data => self%neutron%desilets_n0_cache
    end if
    if (allocated(self%neutron%bulk_density_cache)) then
      self%exchange%bulk_density%data => self%neutron%bulk_density_cache(:, :, land_cover_idx)
    end if
    if (allocated(self%neutron%lattice_water_cache)) then
      self%exchange%lattice_water%data => self%neutron%lattice_water_cache(:, :, land_cover_idx)
    end if
    if (allocated(self%neutron%cosmic_l3_cache)) then
      self%exchange%cosmic_l3%data => self%neutron%cosmic_l3_cache(:, :, land_cover_idx)
    end if
    if (allocated(self%runoff%alpha_cache)) then
      self%exchange%alpha%data => self%runoff%alpha_cache(:, land_cover_idx)
      self%exchange%k_fastflow%data => self%runoff%k_fastflow_cache(:, land_cover_idx)
      self%exchange%k_slowflow%data => self%runoff%k_slowflow_cache(:, land_cover_idx)
      self%exchange%thresh_unsat%data => self%runoff%thresh_unsat_cache
    end if
    if (allocated(self%runoff%k_baseflow_cache)) then
      self%exchange%k_baseflow%data => self%runoff%k_baseflow_cache(:, land_cover_idx)
    end if
    if (allocated(self%runoff%k_percolation_cache)) then
      self%exchange%k_percolation%data => self%runoff%k_percolation_cache(:, land_cover_idx)
    end if
    if (allocated(self%runoff%f_karst_loss_cache)) then
      self%exchange%f_karst_loss%data => self%runoff%f_karst_loss_cache
    end if
    if (allocated(self%runoff%thresh_sealed_cache)) then
      self%exchange%thresh_sealed%data => self%runoff%thresh_sealed_cache
    end if
    self%active_lai_idx = lai_idx
    self%land_cover%active_idx = land_cover_idx
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
        if (self%n_lai_periods >= YEAR_MONTHS) lai_idx = self%exchange%time%month
      case default
        lai_idx = 1_i4
    end select
    lai_idx = max(1_i4, min(lai_idx, self%n_lai_periods))
  end function mpr_lai_index_for_time

  !> \brief Resolve active land-cover period index from current exchange time.
  integer(i4) function mpr_land_cover_index_for_time(self) result(land_cover_idx)
    class(mpr_t), intent(inout), target :: self

    if (self%land_cover%n_periods < 1_i4) then
      log_fatal(*) "MPR: n_land_cover_periods must be >= 1."
      error stop 1
    end if
    if (.not.self%land_cover%temporal) then
      land_cover_idx = 1_i4
      return
    end if
    if (.not.allocated(self%land_cover%period_start)) then
      log_fatal(*) "MPR: temporal land-cover start times are not cached."
      error stop 1
    end if
    if (.not.allocated(self%land_cover%period_end)) then
      log_fatal(*) "MPR: temporal land-cover end times are not cached."
      error stop 1
    end if
    if (self%exchange%time < self%land_cover%period_start(1)) then
      log_fatal(*) "MPR: current time ", self%exchange%time%str(), &
        " is before the first cached land-cover period start ", self%land_cover%period_start(1)%str(), "."
      error stop 1
    end if
    land_cover_idx = max(1_i4, self%land_cover%active_idx)
    do while (land_cover_idx < self%land_cover%n_periods)
      if (self%exchange%time <= self%land_cover%period_end(land_cover_idx)) exit
      land_cover_idx = land_cover_idx + 1_i4
    end do
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
    nullify(self%exchange%f_sealed%data)
    self%exchange%f_sealed%provided = .false.
    nullify(self%exchange%max_interception%data)
    self%exchange%max_interception%provided = .false.
    nullify(self%exchange%thresh_temp%data)
    self%exchange%thresh_temp%provided = .false.
    nullify(self%exchange%degday_dry%data)
    self%exchange%degday_dry%provided = .false.
    nullify(self%exchange%degday_inc%data)
    self%exchange%degday_inc%provided = .false.
    nullify(self%exchange%degday_max%data)
    self%exchange%degday_max%provided = .false.
    nullify(self%exchange%pet_fac_aspect%data)
    self%exchange%pet_fac_aspect%provided = .false.
    nullify(self%exchange%pet_coeff_hs%data)
    self%exchange%pet_coeff_hs%provided = .false.
    nullify(self%exchange%pet_coeff_pt%data)
    self%exchange%pet_coeff_pt%provided = .false.
    nullify(self%exchange%pet_fac_lai%data)
    self%exchange%pet_fac_lai%provided = .false.
    nullify(self%exchange%resist_aero%data)
    self%exchange%resist_aero%provided = .false.
    nullify(self%exchange%resist_surf%data)
    self%exchange%resist_surf%provided = .false.
    nullify(self%exchange%f_roots%data)
    self%exchange%f_roots%provided = .false.
    nullify(self%exchange%sm_saturation%data)
    self%exchange%sm_saturation%provided = .false.
    nullify(self%exchange%sm_exponent%data)
    self%exchange%sm_exponent%provided = .false.
    nullify(self%exchange%sm_field_capacity%data)
    self%exchange%sm_field_capacity%provided = .false.
    nullify(self%exchange%wilting_point%data)
    self%exchange%wilting_point%provided = .false.
    nullify(self%exchange%thresh_jarvis%data)
    self%exchange%thresh_jarvis%provided = .false.
    nullify(self%exchange%desilets_n0%data)
    self%exchange%desilets_n0%provided = .false.
    nullify(self%exchange%bulk_density%data)
    self%exchange%bulk_density%provided = .false.
    nullify(self%exchange%lattice_water%data)
    self%exchange%lattice_water%provided = .false.
    nullify(self%exchange%cosmic_l3%data)
    self%exchange%cosmic_l3%provided = .false.
    nullify(self%exchange%alpha%data)
    self%exchange%alpha%provided = .false.
    nullify(self%exchange%k_fastflow%data)
    self%exchange%k_fastflow%provided = .false.
    nullify(self%exchange%k_slowflow%data)
    self%exchange%k_slowflow%provided = .false.
    nullify(self%exchange%k_baseflow%data)
    self%exchange%k_baseflow%provided = .false.
    nullify(self%exchange%k_percolation%data)
    self%exchange%k_percolation%provided = .false.
    nullify(self%exchange%f_karst_loss%data)
    self%exchange%f_karst_loss%provided = .false.
    nullify(self%exchange%thresh_unsat%data)
    self%exchange%thresh_unsat%provided = .false.
    nullify(self%exchange%thresh_sealed%data)
    self%exchange%thresh_sealed%provided = .false.
    if (self%write_restart) call self%create_restart()
    if (allocated(self%land_cover%ds%vars)) call self%land_cover%ds%close()
    if (allocated(self%slope_emp)) deallocate(self%slope_emp)
    if (allocated(self%land_cover%l0_cache)) deallocate(self%land_cover%l0_cache)
    if (allocated(self%land_cover%forest_fraction_l1)) deallocate(self%land_cover%forest_fraction_l1)
    if (allocated(self%land_cover%sealed_fraction_l1)) deallocate(self%land_cover%sealed_fraction_l1)
    if (allocated(self%land_cover%pervious_fraction_l1)) deallocate(self%land_cover%pervious_fraction_l1)
    if (allocated(self%land_cover%period_start)) deallocate(self%land_cover%period_start)
    if (allocated(self%land_cover%period_end)) deallocate(self%land_cover%period_end)
    if (allocated(self%lai_l0_cache)) deallocate(self%lai_l0_cache)
    if (allocated(self%max_interception_cache)) deallocate(self%max_interception_cache)
    if (allocated(self%snow%thresh_temp_cache)) deallocate(self%snow%thresh_temp_cache)
    if (allocated(self%snow%degday_dry_cache)) deallocate(self%snow%degday_dry_cache)
    if (allocated(self%snow%degday_inc_cache)) deallocate(self%snow%degday_inc_cache)
    if (allocated(self%snow%degday_max_cache)) deallocate(self%snow%degday_max_cache)
    if (allocated(self%pet%pet_fac_aspect_cache)) deallocate(self%pet%pet_fac_aspect_cache)
    if (allocated(self%pet%pet_coeff_hs_cache)) deallocate(self%pet%pet_coeff_hs_cache)
    if (allocated(self%pet%pet_coeff_pt_cache)) deallocate(self%pet%pet_coeff_pt_cache)
    if (allocated(self%pet%pet_fac_lai_cache)) deallocate(self%pet%pet_fac_lai_cache)
    if (allocated(self%pet%resist_aero_cache)) deallocate(self%pet%resist_aero_cache)
    if (allocated(self%pet%resist_surf_cache)) deallocate(self%pet%resist_surf_cache)
    if (allocated(self%soil%sm_exponent_cache)) deallocate(self%soil%sm_exponent_cache)
    if (allocated(self%soil%sm_saturation_cache)) deallocate(self%soil%sm_saturation_cache)
    if (allocated(self%soil%sm_field_capacity_cache)) deallocate(self%soil%sm_field_capacity_cache)
    if (allocated(self%soil%wilting_point_cache)) deallocate(self%soil%wilting_point_cache)
    if (allocated(self%soil%f_roots_cache)) deallocate(self%soil%f_roots_cache)
    if (allocated(self%soil%thresh_jarvis_cache)) deallocate(self%soil%thresh_jarvis_cache)
    if (allocated(self%soil%sm_deficit_fc_l0)) deallocate(self%soil%sm_deficit_fc_l0)
    if (allocated(self%soil%ks_var_h_l0)) deallocate(self%soil%ks_var_h_l0)
    if (allocated(self%soil%ks_var_v_l0)) deallocate(self%soil%ks_var_v_l0)
    if (allocated(self%neutron%desilets_n0_cache)) deallocate(self%neutron%desilets_n0_cache)
    if (allocated(self%neutron%bulk_density_cache)) deallocate(self%neutron%bulk_density_cache)
    if (allocated(self%neutron%lattice_water_cache)) deallocate(self%neutron%lattice_water_cache)
    if (allocated(self%neutron%cosmic_l3_cache)) deallocate(self%neutron%cosmic_l3_cache)
    if (allocated(self%runoff%alpha_cache)) deallocate(self%runoff%alpha_cache)
    if (allocated(self%runoff%k_fastflow_cache)) deallocate(self%runoff%k_fastflow_cache)
    if (allocated(self%runoff%k_slowflow_cache)) deallocate(self%runoff%k_slowflow_cache)
    if (allocated(self%runoff%k_baseflow_cache)) deallocate(self%runoff%k_baseflow_cache)
    if (allocated(self%runoff%k_percolation_cache)) deallocate(self%runoff%k_percolation_cache)
    if (allocated(self%runoff%f_karst_loss_cache)) deallocate(self%runoff%f_karst_loss_cache)
    if (allocated(self%runoff%thresh_unsat_cache)) deallocate(self%runoff%thresh_unsat_cache)
    if (allocated(self%runoff%thresh_sealed_cache)) deallocate(self%runoff%thresh_sealed_cache)
    self%land_cover%temporal = .false.
    self%land_cover%n_periods = 1_i4
    self%land_cover%active_idx = 0_i4
    self%n_lai_periods = 1_i4
    self%active_lai_idx = 0_i4
    log_info(*) "Finalize MPR for domain ", n2s(self%exchange%domain)
  end subroutine mpr_finalize
end module mo_mpr_container
