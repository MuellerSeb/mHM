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
module mo_mhm_container
  use mo_kind, only: i4, dp
  use mo_constants, only : yearmonths
  use mo_mhm_constants, only : noutflxstate
  use mo_optimization_types, only : optidata
  use mo_exchange_type, only: exchange_t
  use mo_message, only: message, error_message
  use nml_config_mhm, only: nml_config_mhm_t, NML_OK

  !> \class   mhm_t
  !> \brief   Class for a single mHM process container.
  type, public :: mhm_t
    type(nml_config_mhm_t) :: config !< configuration of the mHM process container
    type(exchange_t), pointer :: exchange => null() !< exchange container of the domain
    ! DEFINE OUTPUTS
    integer(i4) :: output_deflate_level   !< deflate level in nc files
    integer(i4) :: output_time_reference  !< time reference point location in output nc files
    logical :: output_double_precision    !< output precision in nc files
    integer(i4) :: timeStep_model_outputs !< timestep for writing model outputs
    logical, dimension(nOutFlxState) :: outputFlxState !< Define model outputs see "mhm_outputs.nml"
    ! soil moisture
    real(dp), public, dimension(:, :), allocatable :: L1_sm !< [-] soil moisture input for optimization
    logical, public, dimension(:, :), allocatable :: L1_sm_mask !< [-] mask for valid data in L1_sm
    ! neutrons
    real(dp), public, dimension(:, :), allocatable :: L1_neutronsdata !< [cph] ground albedo neutrons input
    logical, public, dimension(:, :), allocatable :: L1_neutronsdata_mask !< [cph] mask for valid data in L1_neutrons
    ! soil moisture
    integer(i4) :: nSoilHorizons_sm_input ! No. of mhm soil horizons equivalent to sm input

    ! OPTIMIZATION STUFF
    type(optidata), public, dimension(:), allocatable :: L1_smObs
    ! neutrons
    type(optidata), public, dimension(:), allocatable :: L1_neutronsObs
    ! evapotranspiration
    type(optidata), public, dimension(:), allocatable :: L1_etObs
    ! tws
    type(optidata), public, dimension(:), allocatable :: L1_twsaObs !< this stores L1_tws, the mask, the directory of the
                                                              !< observerd data, and the
                                                              !< timestepInput of the simulated data
                                                              ! ToDo: add unit
    logical, public                             :: BFI_calc     !< calculate observed BFI from gauges with Eckhardt filter
    real(dp), public, dimension(:), allocatable :: BFI_obs      !< given base-flow index per domain
    real(dp), public, dimension(:), allocatable :: BFI_qBF_sum  !< q2 weighted sum for each domain
    real(dp), public, dimension(:), allocatable :: BFI_qT_sum   !< q2 weighted sum for each domain

    ! State variables
    ! dim1 = number grid cells L1
    ! dim2 = number model soil horizons
    real(dp), public, dimension(:), allocatable :: L1_inter        !< [mm]  Interception (x1)
    real(dp), public, dimension(:), allocatable :: L1_snowPack     !< [mm]  Snowpack (x2)
    real(dp), public, dimension(:, :), allocatable :: L1_soilMoist !< [mm]  Soil moisture of each horizon (x3)
    real(dp), public, dimension(:), allocatable :: L1_sealSTW      !< [mm]  Retention storage of impervious areas (x4)
    real(dp), public, dimension(:), allocatable :: L1_unsatSTW     !< [mm]  upper soil storage (x5)
    real(dp), public, dimension(:), allocatable :: L1_satSTW       !< [mm]  groundwater storage (x6)
    real(dp), public, dimension(:), allocatable :: L1_neutrons     !< [mm]  Ground Albedo Neutrons

    ! Fluxes
    ! dim1 = number grid cells L1
    ! disaggregated meteo forcings
    real(dp), public, dimension(:), allocatable :: L1_pet_calc     !< [mm TS-1] estimated/corrected potential evapotranspiration
    real(dp), public, dimension(:), allocatable :: L1_temp_calc    !< [degC] temperature for current time step
    real(dp), public, dimension(:), allocatable :: L1_prec_calc    !< [mm TS-1] precipitation for current time step
    ! dim2 = number model soil horizons
    ! states and fluxes
    real(dp), public, dimension(:), allocatable :: L1_percol       !< [mm TS-1] Percolation.
    real(dp), public, dimension(:, :), allocatable :: L1_infilSoil !< [mm TS-1] Infiltration intensity each soil horizon
    real(dp), public, dimension(:, :), allocatable :: L1_aETSoil   !< [mm TS-1] Actual ET from soil layers
    real(dp), public, dimension(:), allocatable :: L1_aETCanopy    !< [mm TS-1] Real evaporation intensity from canopy
    real(dp), public, dimension(:), allocatable :: L1_aETSealed    !< [mm TS-1] Real evap. from free water surfaces
    real(dp), public, dimension(:), allocatable :: L1_melt         !< [mm TS-1] Melting snow depth.
    real(dp), public, dimension(:), allocatable :: L1_preEffect    !< [mm TS-1] Effective precip. depth (snow melt + rain)
    real(dp), public, dimension(:), allocatable :: L1_rain         !< [mm TS-1] Rain precipitation depth
    real(dp), public, dimension(:), allocatable :: L1_snow         !< [mm TS-1] Snow precipitation depth
    real(dp), public, dimension(:), allocatable :: L1_Throughfall  !< [mm TS-1] Throughfall.
    real(dp), public, dimension(:), allocatable :: L1_runoffSeal   !< [mm TS-1] Direct runoff from impervious areas
    real(dp), public, dimension(:), allocatable :: L1_fastRunoff   !< [mm TS-1] Fast runoff component
    real(dp), public, dimension(:), allocatable :: L1_slowRunoff   !< [mm TS-1] Slow runoff component
    real(dp), public, dimension(:), allocatable :: L1_baseflow     !< [mm TS-1] Baseflow
    real(dp), public, dimension(:), allocatable :: L1_total_runoff !< [m3 TS-1] Generated runoff

    real(dp), public, dimension(int(YearMonths, i4)) :: evap_coeff     !< [-] Evap. coef. for free-water surfaces
    real(dp), public, dimension(:), allocatable :: neutron_integral_AFast !< pre-calculated integrand for vertical projection of isotropic neutron flux

  contains
    procedure :: configure => mhm_configure
    procedure :: connect => mhm_connect
    procedure :: initialize => mhm_initialize
    procedure :: update => mhm_update
  end type mhm_t

contains

  !> \brief Configure the mHM process container.
  subroutine mhm_configure(self, file)
    class(mhm_t), intent(inout) :: self
    character(*), intent(in), optional :: file !< file containing the namelists
    character(1024) :: errmsg
    character(:), allocatable :: path
    integer :: status
    call message(" ... configure mhm")
    if (present(file)) then
      path = self%exchange%get_path(file) ! get absolute path relative to cwd
      call message(" ... read mHM config: ", path)
      status = self%config%from_file(file=path, errmsg=errmsg)
      if (status /= NML_OK) call error_message("Error reading mHM config from: ", path, ", with error: ", trim(errmsg))
    end if
    if (.not.self%config%is_configured) call error_message("mHM configuration not set.")
    status = self%config%is_valid(errmsg=errmsg)
    if (status /= NML_OK) call error_message("mHM config not valid. Error: ", trim(errmsg))
  end subroutine mhm_configure

  subroutine mhm_connect(self)
    class(mhm_t), intent(inout) :: self
    call message(" ... connecting mHM: ", self%exchange%time%str())
  end subroutine mhm_connect

  subroutine mhm_initialize(self)
    class(mhm_t), intent(inout) :: self
    call message(" ... initialize mHM: ", self%exchange%time%str())
  end subroutine mhm_initialize

  subroutine mhm_update(self)
    class(mhm_t), intent(inout) :: self
    ! call message(" ... updating mHM: ", self%exchange%time%str())
  end subroutine mhm_update

end module mo_mhm_container
