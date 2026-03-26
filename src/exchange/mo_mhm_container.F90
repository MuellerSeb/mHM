!> \file    mo_mhm_container.f90
!> \brief   \copybrief mo_mhm_container
!> \details \copydetails mo_mhm_container

!> \brief   Module for a mHM process container.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Aug 2025
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_exchange
#include "logging.h"
module mo_mhm_container
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
  use mo_logging
  use mo_kind, only: i4, dp
  use mo_constants, only: YearMonths
  use mo_common_constants, only: P1_InitStateFluxes
  use mo_exchange_type, only: exchange_t, var_dp, var2d_dp
  use mo_datetime, only: one_hour
  use mo_mhm_constants, only: P2_InitStateFluxes, P3_InitStateFluxes, P4_InitStateFluxes, C1_InitStateSM
  use mo_neutrons, only: TabularIntegralAFast
  use mo_string_utils, only: n2s => num2str
  use nml_config_mhm, only: nml_config_mhm_t, NML_OK
  use nml_output_mhm, only: nml_output_mhm_t

  implicit none

  !> \class   mhm_observation_state_t
  !> \brief   Optional optimization-observation arrays owned by the mHM container.
  type :: mhm_observation_state_t
    real(dp), allocatable :: soil_moisture(:, :) !< soil moisture input for optimization
    logical, allocatable :: soil_moisture_mask(:, :) !< valid-data mask for soil moisture optimization input
    real(dp), allocatable :: neutrons(:, :) !< ground albedo neutrons input
    logical, allocatable :: neutrons_mask(:, :) !< valid-data mask for neutron optimization input
    integer(i4) :: n_soil_horizons = 0_i4 !< number of mHM soil horizons represented in optimization input
  end type mhm_observation_state_t

  !> \class   mhm_canopy_state_t
  !> \brief   Grouped canopy process state and fluxes.
  type :: mhm_canopy_state_t
    real(dp), allocatable :: interception(:) !< canopy interception storage
    real(dp), allocatable :: throughfall(:) !< throughfall flux
    real(dp), allocatable :: aet(:) !< actual evapotranspiration from canopy
  end type mhm_canopy_state_t

  !> \class   mhm_snow_state_t
  !> \brief   Grouped snow process state and fluxes.
  type :: mhm_snow_state_t
    real(dp), allocatable :: snowpack(:) !< snowpack storage
    real(dp), allocatable :: melt(:) !< melting snow depth
    real(dp), allocatable :: pre_effect(:) !< effective precipitation depth
    real(dp), allocatable :: rain(:) !< rain precipitation depth
    real(dp), allocatable :: snow(:) !< snow precipitation depth
    real(dp), allocatable :: degday(:) !< degree-day factor
  end type mhm_snow_state_t

  !> \class   mhm_direct_runoff_state_t
  !> \brief   Grouped sealed-area/direct-runoff state and fluxes.
  type :: mhm_direct_runoff_state_t
    real(dp), allocatable :: storage(:) !< retention storage of impervious areas
    real(dp), allocatable :: aet(:) !< actual evapotranspiration from free-water surfaces
    real(dp), allocatable :: runoff(:) !< direct runoff from impervious areas
  end type mhm_direct_runoff_state_t

  !> \class   mhm_soil_state_t
  !> \brief   Grouped soil-moisture state and vertical fluxes.
  type :: mhm_soil_state_t
    real(dp), allocatable :: horizon_bounds(:) !< soil-horizon boundary depths copied from MPR metadata
    real(dp), allocatable :: moisture(:, :) !< soil moisture by horizon
    real(dp), allocatable :: infiltration(:, :) !< infiltration intensity by horizon
    real(dp), allocatable :: aet(:, :) !< actual ET by soil horizon
  end type mhm_soil_state_t

  !> \class   mhm_runoff_state_t
  !> \brief   Grouped storages and runoff/baseflow fluxes.
  type :: mhm_runoff_state_t
    real(dp), allocatable :: unsat_storage(:) !< upper soil storage
    real(dp), allocatable :: sat_storage(:) !< groundwater storage
    real(dp), allocatable :: percolation(:) !< percolation flux
    real(dp), allocatable :: fast_interflow(:) !< fast runoff component
    real(dp), allocatable :: slow_interflow(:) !< slow runoff component
    real(dp), allocatable :: baseflow(:) !< baseflow
    real(dp), allocatable :: total_runoff(:) !< generated runoff
  end type mhm_runoff_state_t

  !> \class   mhm_neutron_state_t
  !> \brief   Grouped neutron state and static support data.
  type :: mhm_neutron_state_t
    real(dp), allocatable :: counts(:) !< simulated ground albedo neutrons
    real(dp), allocatable :: integral_afast(:) !< pre-calculated integrand for neutron flux approximation
  end type mhm_neutron_state_t

  !> \class   mhm_forcing_state_t
  !> \brief   Grouped current-step forcing caches and monthly evaporation coefficients.
  type :: mhm_forcing_state_t
    real(dp), allocatable :: pet(:) !< current-step potential evapotranspiration
    real(dp), allocatable :: temp(:) !< current-step temperature
    real(dp), allocatable :: prec(:) !< current-step precipitation
    real(dp) :: evap_coeff(int(YearMonths, i4)) = 0.0_dp !< monthly evaporation coefficients for free-water surfaces
  end type mhm_forcing_state_t

  !> \class   mhm_io_state_t
  !> \brief   Grouped restart/output bookkeeping owned by the mHM container.
  type :: mhm_io_state_t
    logical :: read_restart = .false. !< whether to read restart data
    logical :: write_restart = .false. !< whether to write restart data
    logical :: output_active = .false. !< whether gridded mHM output is enabled
    character(:), allocatable :: restart_input_path !< resolved restart input path
    character(:), allocatable :: restart_output_path !< resolved restart output path
    character(:), allocatable :: output_path !< resolved output path
  end type mhm_io_state_t

  !> \class   mhm_runtime_state_t
  !> \brief   Grouped scalar runtime bookkeeping for the mHM container.
  type :: mhm_runtime_state_t
    real(dp) :: c2TSTu = 0.0_dp !< conversion factor from model time step to legacy day-based units
  end type mhm_runtime_state_t

  !> \class   mhm_t
  !> \brief   Class for a single mHM process container.
  type, public :: mhm_t
    type(nml_config_mhm_t) :: config !< configuration of the mHM process container
    type(nml_output_mhm_t) :: output_config !< output configuration of the mHM process container
    type(exchange_t), pointer :: exchange => null() !< exchange container of the domain
    type(mhm_observation_state_t) :: observation !< optimization observation support state
    type(mhm_canopy_state_t) :: canopy !< canopy process state and fluxes
    type(mhm_snow_state_t) :: snow !< snow process state and fluxes
    type(mhm_direct_runoff_state_t) :: direct_runoff !< direct-runoff/sealed-area state and fluxes
    type(mhm_soil_state_t) :: soil !< soil-moisture state and vertical fluxes
    type(mhm_runoff_state_t) :: runoff !< runoff/baseflow storages and fluxes
    type(mhm_neutron_state_t) :: neutrons !< neutron state and static support data
    type(mhm_forcing_state_t) :: forcing !< forcing caches and monthly evap coefficients
    type(mhm_io_state_t) :: io !< restart/output bookkeeping
    type(mhm_runtime_state_t) :: runtime !< scalar runtime bookkeeping
  contains
    procedure :: configure => mhm_configure
    procedure :: connect => mhm_connect
    procedure :: initialize => mhm_initialize
    procedure :: update => mhm_update
    procedure :: finalize => mhm_finalize
  end type mhm_t

contains

  !> \brief Configure the mHM process container.
  subroutine mhm_configure(self, file, out_file)
    class(mhm_t), intent(inout), target :: self
    character(*), intent(in), optional :: file !< file containing the namelists
    character(*), intent(in), optional :: out_file !< file containing the output namelists
    integer(i4) :: id(1)
    real(dp) :: l1_res
    character(1024) :: errmsg
    character(:), allocatable :: path
    integer :: status

    log_info(*) "Configure mhm"
    if (present(file)) then
      path = self%exchange%get_path(file)
      log_info(*) "Read mhm config: ", path
      status = self%config%from_file(file=path, errmsg=errmsg)
      if (status /= NML_OK) then
        log_fatal(*) "Error reading mHM config: ", trim(errmsg)
        error stop 1
      end if
    end if
    if (.not.self%config%is_configured) then
      log_fatal(*) "mHM configuration not set."
      error stop 1
    end if
    status = self%config%is_valid(errmsg=errmsg)
    if (status /= NML_OK) then
      log_fatal(*) "mHM config not valid: ", trim(errmsg)
      error stop 1
    end if

    id(1) = self%exchange%domain
    status = self%config%is_set("resolution", idx=id, errmsg=errmsg)
    if (status /= NML_OK) then
      log_fatal(*) "mHM: resolution not set for domain ", n2s(id(1)), ". Error: ", trim(errmsg)
      error stop 1
    end if

    l1_res = self%config%resolution(id(1))
    if (.not.ieee_is_finite(l1_res) .or. l1_res <= 0.0_dp) then
      log_fatal(*) "mHM: resolution must be finite and > 0 for domain ", n2s(id(1)), "."
      error stop 1
    end if
    self%exchange%level1_resolution = l1_res
    log_info(*) "mHM: set level1 resolution for domain ", n2s(id(1)), ": ", n2s(l1_res)

    self%io%read_restart = self%config%read_restart(id(1))
    self%io%write_restart = self%config%write_restart(id(1))

    self%io%output_active = .true.
    if (present(out_file)) then
      log_info(*) "Read mHM output config: ", out_file
      status = self%output_config%from_file(file=out_file, errmsg=errmsg)
      if (status /= NML_OK) then
        self%io%output_active = .false.
        log_warn(*) "mHM output disabled, config not found: ", trim(errmsg)
      end if
    end if
    if (self%output_config%is_configured) then
      status = self%output_config%is_valid(errmsg=errmsg)
      if (status /= NML_OK) then
        log_fatal(*) "mHM output config invalid: ", trim(errmsg)
        error stop 1
      end if
    else
      self%io%output_active = .false.
      log_warn(*) "mHM output disabled, config not set."
    end if

    if (self%io%output_active) then
      status = self%config%is_set("output_path", idx=id, errmsg=errmsg)
      self%io%output_active = self%io%output_active .and. (status == NML_OK)
      if (status /= NML_OK) then
        log_warn(*) "mHM output disabled, path not set for domain ", n2s(id(1)), ": ", trim(errmsg)
      else
        self%io%output_path = self%exchange%get_path(self%config%output_path(id(1)))
      end if
    end if
  end subroutine mhm_configure

  !> \brief Connect the mHM process container with other components.
  subroutine mhm_connect(self)
    class(mhm_t), intent(inout), target :: self
    integer(i4) :: n_cells
    integer(i4) :: n_horizons
    integer(i4) :: interception_case
    integer(i4) :: snow_case
    integer(i4) :: soil_case
    integer(i4) :: direct_runoff_case
    integer(i4) :: interflow_case
    integer(i4) :: baseflow_case
    integer(i4) :: neutron_case

    log_info(*) "Connect mhm"

    if (.not.associated(self%exchange%level1)) then
      log_fatal(*) "mHM: level1 grid not available (check MPR/meteo setup)."
      error stop 1
    end if
    if (.not.associated(self%exchange%soil_horizon_bounds)) then
      log_fatal(*) "mHM: soil_horizon_bounds not provided (check MPR setup)."
      error stop 1
    end if

    call mhm_copy_horizon_bounds(self, self%exchange%soil_horizon_bounds)
    n_cells = int(self%exchange%level1%ncells, i4)
    n_horizons = size(self%soil%horizon_bounds) - 1_i4
    if (n_horizons < 1_i4) then
      log_fatal(*) "mHM: soil_horizon_bounds must contain at least two entries."
      error stop 1
    end if
    self%observation%n_soil_horizons = n_horizons

    interception_case = self%exchange%parameters%process_matrix(1, 1)
    snow_case = self%exchange%parameters%process_matrix(2, 1)
    soil_case = self%exchange%parameters%process_matrix(3, 1)
    direct_runoff_case = self%exchange%parameters%process_matrix(4, 1)
    interflow_case = self%exchange%parameters%process_matrix(6, 1)
    baseflow_case = self%exchange%parameters%process_matrix(9, 1)
    neutron_case = self%exchange%parameters%process_matrix(10, 1)

    call mhm_mark_required_dp(self, self%exchange%pre, &
      (interception_case /= 0_i4) .or. (snow_case /= 0_i4) .or. (soil_case /= 0_i4) .or. (direct_runoff_case /= 0_i4), &
      "pre")
    call mhm_mark_required_dp(self, self%exchange%temp, snow_case /= 0_i4, "temp")
    call mhm_mark_required_dp(self, self%exchange%pet, &
      (interception_case /= 0_i4) .or. (soil_case /= 0_i4) .or. (direct_runoff_case /= 0_i4), "pet")

    call mhm_mark_required_dp(self, self%exchange%max_interception, interception_case /= 0_i4, "max_interception")
    call mhm_mark_required_dp(self, self%exchange%thresh_temp, snow_case /= 0_i4, "thresh_temp")
    call mhm_mark_required_dp(self, self%exchange%degday_dry, snow_case /= 0_i4, "degday_dry")
    call mhm_mark_required_dp(self, self%exchange%degday_inc, snow_case /= 0_i4, "degday_inc")
    call mhm_mark_required_dp(self, self%exchange%degday_max, snow_case /= 0_i4, "degday_max")

    call mhm_mark_required_dp(self, self%exchange%f_sealed, &
      (soil_case /= 0_i4) .or. (direct_runoff_case /= 0_i4) .or. (interflow_case /= 0_i4) .or. (baseflow_case /= 0_i4), &
      "f_sealed")
    call mhm_mark_required_2d(self, self%exchange%f_roots, soil_case /= 0_i4, "f_roots", n_horizons)
    call mhm_mark_required_2d(self, self%exchange%sm_saturation, soil_case /= 0_i4, "sm_saturation", n_horizons)
    call mhm_mark_required_2d(self, self%exchange%sm_exponent, soil_case /= 0_i4, "sm_exponent", n_horizons)
    call mhm_mark_required_2d(self, self%exchange%sm_field_capacity, soil_case /= 0_i4, "sm_field_capacity", n_horizons)
    call mhm_mark_required_2d(self, self%exchange%wilting_point, soil_case /= 0_i4, "wilting_point", n_horizons)
    call mhm_mark_required_dp(self, self%exchange%thresh_jarvis, (soil_case == 2_i4) .or. (soil_case == 3_i4), "thresh_jarvis")
    call mhm_mark_required_dp(self, self%exchange%thresh_sealed, &
      (soil_case /= 0_i4) .or. (direct_runoff_case /= 0_i4), "thresh_sealed")

    call mhm_mark_required_dp(self, self%exchange%alpha, interflow_case /= 0_i4, "alpha")
    call mhm_mark_required_dp(self, self%exchange%k_fastflow, interflow_case /= 0_i4, "k_fastflow")
    call mhm_mark_required_dp(self, self%exchange%k_slowflow, interflow_case /= 0_i4, "k_slowflow")
    call mhm_mark_required_dp(self, self%exchange%k_percolation, interflow_case /= 0_i4, "k_percolation")
    call mhm_mark_required_dp(self, self%exchange%f_karst_loss, interflow_case /= 0_i4, "f_karst_loss")
    call mhm_mark_required_dp(self, self%exchange%thresh_unsat, interflow_case /= 0_i4, "thresh_unsat")
    call mhm_mark_required_dp(self, self%exchange%k_baseflow, baseflow_case /= 0_i4, "k_baseflow")

    call mhm_mark_required_dp(self, self%exchange%desilets_n0, neutron_case /= 0_i4, "desilets_n0")
    call mhm_mark_required_2d(self, self%exchange%bulk_density, neutron_case /= 0_i4, "bulk_density", n_horizons)
    call mhm_mark_required_2d(self, self%exchange%lattice_water, neutron_case /= 0_i4, "lattice_water", n_horizons)
    call mhm_mark_required_2d(self, self%exchange%cosmic_l3, neutron_case == 2_i4, "cosmic_l3", n_horizons)

    call mhm_allocate_1d(self%canopy%interception, n_cells)
    call mhm_allocate_1d(self%canopy%throughfall, n_cells)
    call mhm_allocate_1d(self%canopy%aet, n_cells)

    call mhm_allocate_1d(self%snow%snowpack, n_cells)
    call mhm_allocate_1d(self%snow%melt, n_cells)
    call mhm_allocate_1d(self%snow%pre_effect, n_cells)
    call mhm_allocate_1d(self%snow%rain, n_cells)
    call mhm_allocate_1d(self%snow%snow, n_cells)
    call mhm_allocate_1d(self%snow%degday, n_cells)

    call mhm_allocate_1d(self%direct_runoff%storage, n_cells)
    call mhm_allocate_1d(self%direct_runoff%aet, n_cells)
    call mhm_allocate_1d(self%direct_runoff%runoff, n_cells)

    call mhm_allocate_2d(self%soil%moisture, n_cells, n_horizons)
    call mhm_allocate_2d(self%soil%infiltration, n_cells, n_horizons)
    call mhm_allocate_2d(self%soil%aet, n_cells, n_horizons)

    call mhm_allocate_1d(self%runoff%unsat_storage, n_cells)
    call mhm_allocate_1d(self%runoff%sat_storage, n_cells)
    call mhm_allocate_1d(self%runoff%percolation, n_cells)
    call mhm_allocate_1d(self%runoff%fast_interflow, n_cells)
    call mhm_allocate_1d(self%runoff%slow_interflow, n_cells)
    call mhm_allocate_1d(self%runoff%baseflow, n_cells)
    call mhm_allocate_1d(self%runoff%total_runoff, n_cells)

    call mhm_allocate_1d(self%neutrons%counts, n_cells)

    call mhm_publish_exchange(self)
    call mhm_reset_state_fluxes(self)
  end subroutine mhm_connect

  !> \brief Initialize the mHM process container for the simulation.
  subroutine mhm_initialize(self)
    class(mhm_t), intent(inout), target :: self
    integer(i4) :: coeff_domain
    integer(i4) :: soil_case
    integer(i4) :: direct_runoff_case
    integer(i4) :: neutron_case

    log_info(*) "Initialize mhm"

    if (.not.allocated(self%soil%horizon_bounds)) then
      log_fatal(*) "mHM: soil horizon metadata not initialized before initialize."
      error stop 1
    end if
    if (self%io%read_restart) then
      log_fatal(*) "mHM: restart read is not implemented yet on the exchange path."
      error stop 1
    end if

    coeff_domain = self%exchange%domain
    if (self%config%share_evap_coeff) coeff_domain = 1_i4
    soil_case = self%exchange%parameters%process_matrix(3, 1)
    direct_runoff_case = self%exchange%parameters%process_matrix(4, 1)
    if (soil_case /= 0_i4 .or. direct_runoff_case /= 0_i4) then
      self%forcing%evap_coeff = self%config%evap_coeff(:, coeff_domain)
      if (any(.not.ieee_is_finite(self%forcing%evap_coeff))) then
        log_fatal(*) "mHM: evap_coeff contains invalid values for domain ", n2s(coeff_domain), "."
        error stop 1
      end if
    else
      self%forcing%evap_coeff = 1.0_dp
    end if

    self%runtime%c2TSTu = real(int(self%exchange%step / one_hour(), i4), dp) / 24.0_dp
    if (.not.ieee_is_finite(self%runtime%c2TSTu) .or. self%runtime%c2TSTu <= 0.0_dp) then
      log_fatal(*) "mHM: invalid time-step conversion factor c2TSTu."
      error stop 1
    end if

    neutron_case = self%exchange%parameters%process_matrix(10, 1)
    if (allocated(self%neutrons%integral_afast)) deallocate(self%neutrons%integral_afast)
    if (neutron_case == 2_i4) then
      allocate(self%neutrons%integral_afast(10000 + 2))
      call TabularIntegralAFast(self%neutrons%integral_afast, 20.0_dp)
    else
      allocate(self%neutrons%integral_afast(1))
      self%neutrons%integral_afast = 0.0_dp
    end if

    call mhm_reset_state_fluxes(self)
  end subroutine mhm_initialize

  !> \brief Update the mHM process container for the current time step.
  subroutine mhm_update(self)
    class(mhm_t), intent(inout), target :: self
    log_trace(*) "Update mhm at step ", n2s(self%exchange%step_count)
  end subroutine mhm_update

  !> \brief Finalize the mHM process container after the simulation.
  subroutine mhm_finalize(self)
    class(mhm_t), intent(inout), target :: self
    call mhm_clear_exchange(self)
    if (allocated(self%canopy%interception)) deallocate(self%canopy%interception)
    if (allocated(self%canopy%throughfall)) deallocate(self%canopy%throughfall)
    if (allocated(self%canopy%aet)) deallocate(self%canopy%aet)
    if (allocated(self%snow%snowpack)) deallocate(self%snow%snowpack)
    if (allocated(self%snow%melt)) deallocate(self%snow%melt)
    if (allocated(self%snow%pre_effect)) deallocate(self%snow%pre_effect)
    if (allocated(self%snow%rain)) deallocate(self%snow%rain)
    if (allocated(self%snow%snow)) deallocate(self%snow%snow)
    if (allocated(self%snow%degday)) deallocate(self%snow%degday)
    if (allocated(self%direct_runoff%storage)) deallocate(self%direct_runoff%storage)
    if (allocated(self%direct_runoff%aet)) deallocate(self%direct_runoff%aet)
    if (allocated(self%direct_runoff%runoff)) deallocate(self%direct_runoff%runoff)
    if (allocated(self%soil%horizon_bounds)) deallocate(self%soil%horizon_bounds)
    if (allocated(self%soil%moisture)) deallocate(self%soil%moisture)
    if (allocated(self%soil%infiltration)) deallocate(self%soil%infiltration)
    if (allocated(self%soil%aet)) deallocate(self%soil%aet)
    if (allocated(self%runoff%unsat_storage)) deallocate(self%runoff%unsat_storage)
    if (allocated(self%runoff%sat_storage)) deallocate(self%runoff%sat_storage)
    if (allocated(self%runoff%percolation)) deallocate(self%runoff%percolation)
    if (allocated(self%runoff%fast_interflow)) deallocate(self%runoff%fast_interflow)
    if (allocated(self%runoff%slow_interflow)) deallocate(self%runoff%slow_interflow)
    if (allocated(self%runoff%baseflow)) deallocate(self%runoff%baseflow)
    if (allocated(self%runoff%total_runoff)) deallocate(self%runoff%total_runoff)
    if (allocated(self%neutrons%counts)) deallocate(self%neutrons%counts)
    if (allocated(self%neutrons%integral_afast)) deallocate(self%neutrons%integral_afast)
    self%observation%n_soil_horizons = 0_i4
    self%runtime%c2TSTu = 0.0_dp
    log_info(*) "Finalize mhm"
  end subroutine mhm_finalize

  !> \brief Mark a 1D exchange field as required and validate it.
  subroutine mhm_mark_required_dp(self, var, required, name)
    class(mhm_t), intent(in) :: self
    type(var_dp), intent(inout) :: var
    logical, intent(in) :: required
    character(*), intent(in) :: name

    var%required = required
    if (.not.required) return
    if (.not.var%provided) then
      log_fatal(*) "mHM: ", trim(name), " not provided."
      error stop 1
    end if
    if (.not.associated(var%data)) then
      log_fatal(*) "mHM: ", trim(name), " data not connected."
      error stop 1
    end if
    if (size(var%data, 1) /= self%exchange%level1%ncells) then
      log_fatal(*) "mHM: ", trim(name), " has unexpected level1 size."
      error stop 1
    end if
  end subroutine mhm_mark_required_dp

  !> \brief Mark a 2D exchange field as required and validate its soil-horizon shape.
  subroutine mhm_mark_required_2d(self, var, required, name, n_horizons)
    class(mhm_t), intent(in) :: self
    type(var2d_dp), intent(inout) :: var
    logical, intent(in) :: required
    character(*), intent(in) :: name
    integer(i4), intent(in) :: n_horizons

    var%required = required
    if (.not.required) return
    if (.not.var%provided) then
      log_fatal(*) "mHM: ", trim(name), " not provided."
      error stop 1
    end if
    if (.not.associated(var%data)) then
      log_fatal(*) "mHM: ", trim(name), " data not connected."
      error stop 1
    end if
    if (size(var%data, 1) /= self%exchange%level1%ncells .or. size(var%data, 2) /= n_horizons) then
      log_fatal(*) "mHM: ", trim(name), " has unexpected soil-horizon shape."
      error stop 1
    end if
  end subroutine mhm_mark_required_2d

  !> \brief Allocate or resize a 1D state array.
  subroutine mhm_allocate_1d(data, n_cells)
    real(dp), allocatable, intent(inout) :: data(:)
    integer(i4), intent(in) :: n_cells

    if (allocated(data)) then
      if (size(data) /= n_cells) deallocate(data)
    end if
    if (.not.allocated(data)) allocate(data(n_cells))
  end subroutine mhm_allocate_1d

  !> \brief Allocate or resize a 2D state array.
  subroutine mhm_allocate_2d(data, n_cells, n_horizons)
    real(dp), allocatable, intent(inout) :: data(:, :)
    integer(i4), intent(in) :: n_cells
    integer(i4), intent(in) :: n_horizons

    if (allocated(data)) then
      if (size(data, 1) /= n_cells .or. size(data, 2) /= n_horizons) deallocate(data)
    end if
    if (.not.allocated(data)) allocate(data(n_cells, n_horizons))
  end subroutine mhm_allocate_2d

  !> \brief Copy soil-horizon metadata from the exchange contract into container-owned state.
  subroutine mhm_copy_horizon_bounds(self, bounds)
    class(mhm_t), intent(inout), target :: self
    real(dp), dimension(:), pointer, intent(in) :: bounds
    integer(i4) :: i

    if (size(bounds) < 2_i4) then
      log_fatal(*) "mHM: soil_horizon_bounds must contain at least two entries."
      error stop 1
    end if
    if (allocated(self%soil%horizon_bounds)) then
      if (size(self%soil%horizon_bounds) /= size(bounds)) deallocate(self%soil%horizon_bounds)
    end if
    if (.not.allocated(self%soil%horizon_bounds)) allocate(self%soil%horizon_bounds(size(bounds)))
    self%soil%horizon_bounds = bounds
    do i = 2_i4, size(self%soil%horizon_bounds)
      if (.not.ieee_is_finite(self%soil%horizon_bounds(i)) .or. &
        self%soil%horizon_bounds(i) <= self%soil%horizon_bounds(i - 1_i4)) then
        log_fatal(*) "mHM: soil_horizon_bounds must be finite and strictly increasing."
        error stop 1
      end if
    end do
  end subroutine mhm_copy_horizon_bounds

  !> \brief Publish mHM-owned state and flux arrays through the exchange contract.
  subroutine mhm_publish_exchange(self)
    class(mhm_t), intent(inout), target :: self

    self%exchange%interception%data => self%canopy%interception
    self%exchange%interception%provided = .true.
    self%exchange%throughfall%data => self%canopy%throughfall
    self%exchange%throughfall%provided = .true.
    self%exchange%aet_canopy%data => self%canopy%aet
    self%exchange%aet_canopy%provided = .true.

    self%exchange%snowpack%data => self%snow%snowpack
    self%exchange%snowpack%provided = .true.
    self%exchange%melt%data => self%snow%melt
    self%exchange%melt%provided = .true.
    self%exchange%pre_eff%data => self%snow%pre_effect
    self%exchange%pre_eff%provided = .true.
    self%exchange%rain%data => self%snow%rain
    self%exchange%rain%provided = .true.
    self%exchange%snow%data => self%snow%snow
    self%exchange%snow%provided = .true.
    self%exchange%degday%data => self%snow%degday
    self%exchange%degday%provided = .true.

    self%exchange%sealed_storage%data => self%direct_runoff%storage
    self%exchange%sealed_storage%provided = .true.
    self%exchange%aet_sealed%data => self%direct_runoff%aet
    self%exchange%aet_sealed%provided = .true.
    self%exchange%runoff_sealed%data => self%direct_runoff%runoff
    self%exchange%runoff_sealed%provided = .true.

    self%exchange%soil_moisture%data => self%soil%moisture
    self%exchange%soil_moisture%provided = .true.
    self%exchange%infiltration%data => self%soil%infiltration
    self%exchange%infiltration%provided = .true.
    self%exchange%aet_soil%data => self%soil%aet
    self%exchange%aet_soil%provided = .true.

    self%exchange%unsat_storage%data => self%runoff%unsat_storage
    self%exchange%unsat_storage%provided = .true.
    self%exchange%sat_storage%data => self%runoff%sat_storage
    self%exchange%sat_storage%provided = .true.
    self%exchange%percolation%data => self%runoff%percolation
    self%exchange%percolation%provided = .true.
    self%exchange%interflow_fast%data => self%runoff%fast_interflow
    self%exchange%interflow_fast%provided = .true.
    self%exchange%interflow_slow%data => self%runoff%slow_interflow
    self%exchange%interflow_slow%provided = .true.
    self%exchange%baseflow%data => self%runoff%baseflow
    self%exchange%baseflow%provided = .true.
    self%exchange%runoff_total%data => self%runoff%total_runoff
    self%exchange%runoff_total%provided = .true.

    self%exchange%neutrons%data => self%neutrons%counts
    self%exchange%neutrons%provided = .true.
  end subroutine mhm_publish_exchange

  !> \brief Reset all mHM state and flux arrays to legacy default values.
  subroutine mhm_reset_state_fluxes(self)
    class(mhm_t), intent(inout), target :: self
    integer(i4) :: horizon
    real(dp) :: layer_depth

    if (.not.allocated(self%soil%horizon_bounds)) then
      log_fatal(*) "mHM: soil horizon bounds not available for state initialization."
      error stop 1
    end if

    self%canopy%interception = P1_InitStateFluxes
    self%canopy%throughfall = P1_InitStateFluxes
    self%canopy%aet = P1_InitStateFluxes

    self%snow%snowpack = P2_InitStateFluxes
    self%snow%melt = P1_InitStateFluxes
    self%snow%pre_effect = P1_InitStateFluxes
    self%snow%rain = P1_InitStateFluxes
    self%snow%snow = P1_InitStateFluxes
    self%snow%degday = P1_InitStateFluxes

    self%direct_runoff%storage = P1_InitStateFluxes
    self%direct_runoff%aet = P1_InitStateFluxes
    self%direct_runoff%runoff = P1_InitStateFluxes

    self%soil%infiltration = P1_InitStateFluxes
    self%soil%aet = P1_InitStateFluxes
    do horizon = 1_i4, size(self%soil%moisture, 2)
      layer_depth = self%soil%horizon_bounds(horizon + 1_i4) - self%soil%horizon_bounds(horizon)
      if (.not.ieee_is_finite(layer_depth) .or. layer_depth <= 0.0_dp) then
        log_fatal(*) "mHM: soil horizon depth must be finite and > 0."
        error stop 1
      end if
      self%soil%moisture(:, horizon) = layer_depth * C1_InitStateSM
    end do

    self%runoff%unsat_storage = P3_InitStateFluxes
    self%runoff%sat_storage = P4_InitStateFluxes
    self%runoff%percolation = P1_InitStateFluxes
    self%runoff%fast_interflow = P1_InitStateFluxes
    self%runoff%slow_interflow = P1_InitStateFluxes
    self%runoff%baseflow = P1_InitStateFluxes
    self%runoff%total_runoff = P1_InitStateFluxes

    self%neutrons%counts = P1_InitStateFluxes
  end subroutine mhm_reset_state_fluxes

  !> \brief Clear mHM-owned exchange publications.
  subroutine mhm_clear_exchange(self)
    class(mhm_t), intent(inout), target :: self

    nullify(self%exchange%interception%data)
    self%exchange%interception%provided = .false.
    nullify(self%exchange%throughfall%data)
    self%exchange%throughfall%provided = .false.
    nullify(self%exchange%aet_canopy%data)
    self%exchange%aet_canopy%provided = .false.
    nullify(self%exchange%snowpack%data)
    self%exchange%snowpack%provided = .false.
    nullify(self%exchange%melt%data)
    self%exchange%melt%provided = .false.
    nullify(self%exchange%pre_eff%data)
    self%exchange%pre_eff%provided = .false.
    nullify(self%exchange%rain%data)
    self%exchange%rain%provided = .false.
    nullify(self%exchange%snow%data)
    self%exchange%snow%provided = .false.
    nullify(self%exchange%degday%data)
    self%exchange%degday%provided = .false.
    nullify(self%exchange%sealed_storage%data)
    self%exchange%sealed_storage%provided = .false.
    nullify(self%exchange%aet_sealed%data)
    self%exchange%aet_sealed%provided = .false.
    nullify(self%exchange%runoff_sealed%data)
    self%exchange%runoff_sealed%provided = .false.
    nullify(self%exchange%soil_moisture%data)
    self%exchange%soil_moisture%provided = .false.
    nullify(self%exchange%infiltration%data)
    self%exchange%infiltration%provided = .false.
    nullify(self%exchange%aet_soil%data)
    self%exchange%aet_soil%provided = .false.
    nullify(self%exchange%unsat_storage%data)
    self%exchange%unsat_storage%provided = .false.
    nullify(self%exchange%sat_storage%data)
    self%exchange%sat_storage%provided = .false.
    nullify(self%exchange%percolation%data)
    self%exchange%percolation%provided = .false.
    nullify(self%exchange%interflow_fast%data)
    self%exchange%interflow_fast%provided = .false.
    nullify(self%exchange%interflow_slow%data)
    self%exchange%interflow_slow%provided = .false.
    nullify(self%exchange%baseflow%data)
    self%exchange%baseflow%provided = .false.
    nullify(self%exchange%runoff_total%data)
    self%exchange%runoff_total%provided = .false.
    nullify(self%exchange%neutrons%data)
    self%exchange%neutrons%provided = .false.
  end subroutine mhm_clear_exchange

end module mo_mhm_container
