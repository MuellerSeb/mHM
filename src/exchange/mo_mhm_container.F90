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
  use mo_exchange_type, only: exchange_t
  use mo_string_utils, only: n2s => num2str
  use mo_message, only: error_message
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
    if (.not.self%config%is_configured) call error_message("mHM configuration not set.")
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
    log_info(*) "Connect mhm"
  end subroutine mhm_connect

  !> \brief Initialize the mHM process container for the simulation.
  subroutine mhm_initialize(self)
    class(mhm_t), intent(inout), target :: self
    log_info(*) "Initialize mhm"
  end subroutine mhm_initialize

  !> \brief Update the mHM process container for the current time step.
  subroutine mhm_update(self)
    class(mhm_t), intent(inout), target :: self
    log_trace(*) "Update mhm"
  end subroutine mhm_update

  !> \brief Finalize the mHM process container after the simulation.
  subroutine mhm_finalize(self)
    class(mhm_t), intent(inout), target :: self
    log_info(*) "Finalize mhm"
  end subroutine mhm_finalize

end module mo_mhm_container
