!> \dir exchange
!> \brief \copybrief f_exchange
!> \details \copydetails f_exchange

!> \defgroup   f_exchange exchange - Fortran modules
!> \brief      Modules to deal with data exchange between mHM components.
!> \details    This module provides different types to enable the data exchange between components.

!> \file    mo_exchange_type.f90
!> \brief   \copybrief mo_exchange_type
!> \details \copydetails mo_exchange_type

!> \brief   Module to provide the exchange type.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Mar 2025
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_exchange
module mo_exchange_type
  use mo_common_types, only: grid_t => grid
  use mo_datetime, only: datetime
  use mo_kind, only: dp, i4
  use mo_message, only: error_message
  use mo_string_utils, only: n2s=>num2str
  implicit none
  private
  integer(i4), public, parameter :: nogrid = 0_i4 !< no grid (yet) defined
  integer(i4), public, parameter :: l0 = 1_i4 !< level0 - morphology
  integer(i4), public, parameter :: l1 = 2_i4 !< level1 - hydrology
  integer(i4), public, parameter :: l2 = 3_i4 !< level2 - meteorology
  integer(i4), public, parameter :: l11 = 4_i4 !< level11 - routing

  type :: named_grid
    character(:), allocatable :: name !< grid name ("L0", "L1", "L2", "L11")
    type(grid_t), pointer :: grid => null() !< horizontal grid the data is defined on
  end type named_grid

  type, abstract :: variable_abc
    character(:), allocatable :: name !< variable name
    character(:), allocatable :: unit !< variable unit
    character(:), allocatable :: long_name !< long name of the variable
    character(:), allocatable :: standard_name !< standard name of the variable
    integer(i4) :: grid = nogrid !< ID of the grid the data is defined on
    logical :: static = .false. !< flag to indicated static data (.false. by default)
    logical :: provided = .false. !< flag to indicate that data is provided by a component (.false. by default)
    logical :: required = .false. !< flag to indicate that data is required by a component (.false. by default)
  end type variable_abc

  type, extends(variable_abc) :: var_dp
    real(dp), dimension(:), pointer :: data => null() !< 1D real pointer (n-cells)
  end type var_dp

  type, extends(variable_abc) :: var_i4
    integer(i4), dimension(:), pointer :: data => null() !< 1D integer pointer (n-cells)
  end type var_i4

  type, extends(variable_abc) :: var_lg
    logical, dimension(:), pointer :: data => null() !< 1D logical pointer (n-cells)
  end type var_lg

  type, extends(variable_abc) :: var2d_dp
    real(dp), dimension(:,:), pointer :: data => null() !< 2D real pointer (n-cells, horizons)
  end type var2d_dp

  type, extends(variable_abc) :: var2d_i4
    integer(i4), dimension(:,:), pointer :: data => null() !< 2D integer pointer (n-cells, horizons)
  end type var2d_i4

  type, extends(variable_abc) :: var2d_lg
    logical, dimension(:,:), pointer :: data => null() !< 2D logical pointer (n-cells, horizons)
  end type var2d_lg

  type, public :: exchange_t
    integer(i4) :: time_step !< current time step
    type(datetime) :: current_time !< time-stamp for the current time step

    ! grids
    type(named_grid) :: level0  !< level0 grid of the morphology
    type(named_grid) :: level1  !< level1 grid of the hydrology
    type(named_grid) :: level2  !< level2 grid of the meteorology
    type(named_grid) :: level11 !< level11 grid of the river network

    ! variables
    ! meteorology (level1)
    type(var_dp) :: pre  !< precipitation [mm] on level l1
    type(var_dp) :: temp !< air temperature [degC] on level l1
    type(var_dp) :: pet  !< potential evapotranspiration [mm] on level l1
    type(var_dp) :: ssrd !< solar short wave radiation downward [W m-2] on level l1
    type(var_dp) :: strd !< surface thermal radiation downward [W m-2] on level l1
    type(var_dp) :: tann !< annual mean air temperature [degC] on level l1

    ! meteorology (level2)
    ! type(var_dp) :: tmin   !< minimum daily temperature [degC] on level l2
    ! type(var_dp) :: tmax   !< maximum daily temperature [degC] on level l2
    ! type(var_dp) :: netrad !< net radiation [W m-2] on level l2
    ! type(var_dp) :: eabs   !< vapor pressure [Pa] on level l2
    ! type(var_dp) :: wind   !< wind speed [m s-1] on level l2

    ! morphology (level0)
    type(var_dp) :: dem    !< elevation [m] on level l0 (static)
    type(var_dp) :: slope  !< slope [%] on level l0 (static)
    type(var_dp) :: aspect !< aspect [degree] on level l0 (static)

    ! hydrology (level1)
    ! canopy
    type(var_dp) :: interception !< canopy interception storage [mm] on level l1
    type(var_dp) :: throughfall  !< throughfall amount [mm] on level l1
    ! storage and SM
    type(var2d_dp) :: soil_moisture !< soil water content of soil layer [mm] on level l1
    type(var_dp) :: sealed_storage  !< reservoir of sealed areas [mm] on level l1
    type(var_dp) :: unsat_storage   !< reservoir of unsaturated zone [mm] on level l1
    type(var_dp) :: sat_storage     !< water level in groundwater reservoir [mm] on level l1
    ! AET
    type(var_dp) :: aet_canopy !< actual evapotranspiration from canopy [mm] on level l1
    type(var_dp) :: aet_sealed !< actual evapotranspiration from free water surfaces [mm] on level l1
    type(var2d_dp) :: aet_soil !< actual evapotranspiration from soil layer [mm] on level l1
    ! rain/snow
    type(var_dp) :: snowpack !< depth of snowpack [mm] on level l1
    type(var_dp) :: rain     !< rain precipitation [mm] on level l1
    type(var_dp) :: snow     !< snow precipitation [mm] on level l1
    type(var_dp) :: melt     !< melting snow [mm] on level l1
    type(var_dp) :: pre_eff  !< effective precipitation [mm] on level l1 (rain + melt)
    ! vertical soil water movement
    type(var2d_dp) :: infiltration !< infiltration intensity in soil layer [mm] on level l1
    type(var_dp) :: percolation    !< percolation [mm] on level l1
    ! type(var_dp) :: loss !< gain/loss flux in a leaking linear reservoir [mm] on level l1
    ! lateral water movement
    type(var_dp) :: runoff_total   !< total runoff [mm] on level l1
    type(var_dp) :: runoff_sealed  !< direct runoff from impervious areas [mm] on level l1
    type(var_dp) :: interflow_fast !< fast runoff component [mm] on level l1
    type(var_dp) :: interflow_slow !< slow runoff component [mm] on level l1
    type(var_dp) :: baseflow       !< baseflow [mm] on level l1
    ! neutrons
    type(var_dp) :: neutrons !< ground albedo neutrons [cph] on level l1
    ! degday calculated by mHM from MPR degday_X variables
    type(var_dp) :: degday !< Degree-day factor [mm TS-1 degC-1] on level l1

    ! MPR results (level1)
    type(var_dp) :: alpha               !< Exponent for the upper reservoir [1] on level l1
    type(var_dp) :: degday_inc          !< Increase of the degree-day factor per precipitation [TS-1 degC-1] on level l1
    type(var_dp) :: degday_max          !< Maximum degree-day factor [mm TS-1 degC-1] on level l1
    type(var_dp) :: degday_dry          !< Degree-day factor for no precipitation [mm TS-1 degC-1] on level l1
    type(var2d_dp) :: f_roots           !< Fraction of roots in soil horizons [1] on level l1
    type(var_dp) :: f_sealed            !< Fraction of sealed area [1] on level l1
    type(var_dp) :: f_karst_loss        !< Fraction of karstic percolation loss [1] on level l1
    type(var_dp) :: pet_fac_aspect      !< PET correction based on aspect [1] on level l1
    type(var_dp) :: pet_fac_lai         !< PET correction based on LAI [1] on level l1
    type(var_dp) :: pet_coeff_hs        !< PET calculation coefficient for Hargreaves Samani [1] on level l1
    type(var_dp) :: pet_coeff_pt        !< PET calculation coefficient for Priestley Taylor (alpha) [1] on level l1
    type(var_dp) :: resist_aero         !< aerodynamical resistance [s m-1] on level l1
    type(var_dp) :: resist_surf         !< bulk surface resistance [s m-1] on level l1
    type(var_dp) :: k_fastflow          !< Fast interflow recession coefficient [TS-1] on level l1
    type(var_dp) :: k_slowflow          !< Slow interflow recession coefficient [TS-1] on level l1
    type(var_dp) :: k_baseflow          !< Baseflow recession coefficient [TS-1] on level l1
    type(var_dp) :: k_percolation       !< Percolation coefficient [TS-1] on level l1
    type(var2d_dp) :: sm_saturation     !< Saturation soil moisture [mm] on level l1
    type(var2d_dp) :: sm_exponent       !< Exponential parameter controlling non-linearity of soil water retention [1] on level l1
    type(var2d_dp) :: sm_field_capacity !< Field capacity - soil moisture below which actual ET is reduced [mm] on level l1
    type(var2d_dp) :: wilting_point     !< permanent wilting point [mm] on level l1
    type(var_dp) :: thresh_temp         !< Threshold temperature for phase transition snow and rain [degC] on level l1
    type(var_dp) :: thresh_unsat        !< Threshold water depth for fast interflow [mm] on level l1
    type(var_dp) :: thresh_sealed       !< Threshold water depth for runoff on sealed surfaces [mm] on level l1
    type(var_dp) :: thresh_jarvis       !< Jarvis critical value (C1) for normalized soil water content [1] on level l1
    type(var_dp) :: max_interception    !< Maximum interception [mm] on level l1
    type(var_dp) :: neutron_ref_count   !< neutron count rate under dry reference conditions (N_0) [1] on level l1
    type(var2d_dp) :: bulk_density      !< bulk density [g cm-3] on level l1
    type(var2d_dp) :: lattice_water     !< Ratio of structurally bound water [1] on level l1
    type(var2d_dp) :: cosmic_l3         !< cosmic L3 parameter [1] on level l1

    ! routing (level11)
    type(var_dp) :: q_out               !< accumulated runoff [m3 s-1] on level l11
    type(var_dp) :: q_mod               !< modelled discharge [m3 s-1] on level l11

  end type exchange_t

contains

  subroutine exchange_init(self, start_time)
    type(exchange_t), intent(inout) :: self
    type(datetime), intent(in) :: start_time

    ! time
    self%time_step = 0_i4
    self%current_time = start_time

    ! grids
    self%level0 = named_grid(name="L0")
    self%level1 = named_grid(name="L1")
    self%level2 = named_grid(name="L2")
    self%level11 = named_grid(name="L11")

    ! variables
    ! meteorology (level1)
    self%pre  = var_dp(grid=l1, name="pre",  unit="mm",    long_name="precipitation", standard_name="precipitation_amount")
    self%temp = var_dp(grid=l1, name="temp", unit="degC",  long_name="air temperature", standard_name="air_temperature")
    self%pet  = var_dp(grid=l1, name="pet",  unit="mm",    long_name="potential evapotranspiration", standard_name="water_potential_evapotranspiration_amount")
    self%ssrd = var_dp(grid=l1, name="ssrd", unit="W m-2", long_name="solar short wave radiation downward", standard_name="surface_downwelling_shortwave_flux")
    self%strd = var_dp(grid=l1, name="strd", unit="W m-2", long_name="surface thermal radiation downward", standard_name="surface_downwelling_longwave_flux")
    self%tann = var_dp(grid=l1, name="tann", unit="degC",  long_name="annual mean air temperature", standard_name="air_temperature")

    ! meteorology (level2)
    ! self%tmin   = var_dp(grid=l2, name="tmin",      unit="degC", long_name="minimum daily temperature", standard_name="air_temperature")
    ! self%tmax   = var_dp(grid=l2, name="tmax",      unit="degC", long_name="maximum daily temperature", standard_name="air_temperature")
    ! self%netrad = var_dp(grid=l2, name="netrad",    unit="W m-2", long_name="net radiation", standard_name="surface_net_downward_radiative_flux")
    ! self%eabs   = var_dp(grid=l2, name="eabs",      unit="Pa", long_name="vapor pressure", standard_name="water_vapor_pressure")
    ! self%wind   = var_dp(grid=l2, name="windspeed", unit="m s-1", long_name="wind speed", standard_name="wind_speed")

    ! morphology (level0)
    self%dem    = var_dp(static=.true., grid=l0, name="dem",    unit="m",      long_name="elevation", standard_name="height_above_mean_sea_level")
    self%slope  = var_dp(static=.true., grid=l0, name="slope",  unit="%",      long_name="slope", standard_name="ground_slope_angle")
    self%aspect = var_dp(static=.true., grid=l0, name="aspect", unit="degree", long_name="aspect", standard_name="ground_slope_direction")

    ! hydrology (level1)
    ! canopy
    self%interception = var_dp(grid=l1, name="interception", unit="mm", long_name="canopy interception storage")
    self%throughfall  = var_dp(grid=l1, name="throughfall",  unit="mm", long_name="throughfall amount")
    ! storage and SM
    self%soil_moisture = var2d_dp(grid=l1, name="soil_moisture", unit="mm", long_name="soil water content of soil layer")
    self%sealed_storage  = var_dp(grid=l1, name="sealedSTW",     unit="mm", long_name="reservoir of sealed areas")
    self%unsat_storage   = var_dp(grid=l1, name="unsatSTW",      unit="mm", long_name="reservoir of unsaturated zone")
    self%sat_storage     = var_dp(grid=l1, name="satSTW",        unit="mm", long_name="water level in groundwater reservoir")
    ! AET
    self%aet_canopy = var_dp(grid=l1, name="aet_canopy", unit="mm", long_name="actual evapotranspiration from canopy")
    self%aet_sealed = var_dp(grid=l1, name="aet_sealed", unit="mm", long_name="actual evapotranspiration from free water surfaces")
    self%aet_soil = var2d_dp(grid=l1, name="aet_soil",   unit="mm", long_name="actual evapotranspiration from soil layer")
    ! rain/snow
    self%snowpack = var_dp(grid=l1, name="snowpack", unit="mm", long_name="depth of snowpack", standard_name="surface_snow_amount")
    self%rain     = var_dp(grid=l1, name="rain",     unit="mm", long_name="rain precipitation", standard_name="rainfall_amount")
    self%snow     = var_dp(grid=l1, name="snow",     unit="mm", long_name="snow precipitation", standard_name="snowfall_amount")
    self%melt     = var_dp(grid=l1, name="melt",     unit="mm", long_name="melting snow", standard_name="surface_snow_melt_amount")
    self%pre_eff  = var_dp(grid=l1, name="pre_eff",  unit="mm", long_name="effective precipitation") ! rain + melt
    ! vertical soil water movement
    self%infiltration = var2d_dp(grid=l1, name="infiltration", unit="mm", long_name="infiltration intensity in soil layer")
    self%percolation  =   var_dp(grid=l1, name="percolation",  unit="mm", long_name="percolation")
    ! lateral water movement
    self%runoff_total   = var_dp(grid=l1, name="Q",   unit="mm", long_name="total runoff", standard_name="runoff_amount")
    self%runoff_sealed  = var_dp(grid=l1, name="QD",  unit="mm", long_name="direct runoff from impervious areas", standard_name="surface_runoff_amount")
    self%interflow_fast = var_dp(grid=l1, name="QIf", unit="mm", long_name="fast runoff component", standard_name="subsurface_runoff_amount")
    self%interflow_slow = var_dp(grid=l1, name="QIs", unit="mm", long_name="slow runoff component", standard_name="subsurface_runoff_amount")
    self%baseflow       = var_dp(grid=l1, name="QB",  unit="mm", long_name="baseflow" standard_name="baseflow_amount")
    ! neutrons
    self%neutrons = var_dp(grid=l1, name="neutrons", unit="cph", long_name="ground albedo neutrons")

    ! MPR results (level1)


    ! routing (level11)



  end subroutine exchange_init

end module mo_exchange_type
