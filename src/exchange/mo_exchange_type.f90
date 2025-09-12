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
  use mo_common_constants, only: nprocesses
  use mo_datetime, only: datetime, timedelta
  use mo_kind, only: dp, i4
  use mo_message, only: message, error_message
  use mo_string_utils, only: n2s=>num2str

  use mo_namelists, only: &
    nml_mainconfig_mhm_mrm_t, &
    nml_time_periods_t, &
    nml_processselection_t, &
    nml_interception1_t, &
    nml_snow1_t, &
    nml_soilmoisture1_t, &
    nml_soilmoisture2_t, &
    nml_soilmoisture3_t, &
    nml_soilmoisture4_t, &
    nml_directrunoff1_t, &
    nml_PETminus1_t, &
    nml_PET0_t, &
    nml_PET1_t, &
    nml_PET2_t, &
    nml_PET3_t, &
    nml_interflow1_t, &
    nml_percolation1_t, &
    nml_routing1_t, &
    nml_routing2_t, &
    nml_routing3_t, &
    nml_neutrons1_t, &
    nml_neutrons2_t, &
    nml_geoparameter_t

  implicit none
  private

  !> \name Level Selectors
  !> \brief Constants to specify the grid for levels in mHM: L0, L1, L2 and L11.
  !!@{
  integer(i4), public, parameter :: nogrid = -1_i4 !< no grid (yet) defined
  integer(i4), public, parameter :: l0 = 0_i4      !< level0 - morphology
  integer(i4), public, parameter :: l1 = 1_i4      !< level1 - hydrology
  integer(i4), public, parameter :: l2 = 2_i4      !< level2 - meteorology
  integer(i4), public, parameter :: l11 = 3_i4     !< level11 - routing
  !!@}

  !> \class   parameter_config_t
  !> \brief   Configuration for all parameters.
  type, public :: parameter_config_t
    character(:), allocatable :: file          !< parameter namelist file
    type(nml_interception1_t) :: interception1 !< interception1 configuration
    type(nml_snow1_t) :: snow1 !< snow1 configuration
    type(nml_soilmoisture1_t) :: soilmoisture1 !< soilmoisture1 configuration
    type(nml_soilmoisture2_t) :: soilmoisture2 !< soilmoisture2 configuration
    type(nml_soilmoisture3_t) :: soilmoisture3 !< soilmoisture3 configuration
    type(nml_soilmoisture4_t) :: soilmoisture4 !< soilmoisture4 configuration
    type(nml_directrunoff1_t) :: directrunoff1 !< directrunoff1 configuration
    type(nml_PETminus1_t) :: PETminus1 !< PETminus1 configuration
    type(nml_PET0_t) :: PET0 !< PET0 configuration
    type(nml_PET1_t) :: PET1 !< PET1 configuration
    type(nml_PET2_t) :: PET2 !< PET2 configuration
    type(nml_PET3_t) :: PET3 !< PET3 configuration
    type(nml_interflow1_t) :: interflow1 !< interflow1 configuration
    type(nml_percolation1_t) :: percolation1 !< percolation1 configuration
    type(nml_routing1_t) :: routing1 !< routing1 configuration
    type(nml_routing2_t) :: routing2 !< routing2 configuration
    type(nml_routing3_t) :: routing3 !< routing3 configuration
    type(nml_neutrons1_t) :: neutrons1 !< neutrons1 configuration
    type(nml_neutrons2_t) :: neutrons2 !< neutrons2 configuration
    type(nml_geoparameter_t) :: geoparameter !< geoparameter configuration
  contains
    procedure :: read => parameter_config_read
  end type parameter_config_t

  !> \class   process_config_t
  !> \brief   Configuration for all processes.
  type, public :: process_config_t
    character(:), allocatable :: file          !< mhm namelist file
    type(nml_processselection_t) :: processselection !< processselection configuration
  contains
    procedure :: read => process_config_read
  end type process_config_t

  !> \class   time_config_t
  !> \brief   Configuration for time periods.
  type, public :: time_config_t
    integer(i4) :: domain !< domain number to read correct configuration
    character(:), allocatable :: file          !< mhm namelist file
    type(nml_mainconfig_mhm_mrm_t) :: mainconfig_mhm_mrm !< mainconfig_mhm_mrm configuration containing time step
    type(nml_time_periods_t) :: time_periods !< time_periods configuration
  contains
    procedure :: read => time_config_read
  end type time_config_t

  !> \class   variable_abc
  !> \brief   Abstract base class for a variable in the exchange type.
  type, abstract :: variable_abc
    character(:), allocatable :: name          !< variable name
    character(:), allocatable :: units         !< variable unit
    character(:), allocatable :: long_name     !< long name of the variable
    character(:), allocatable :: standard_name !< standard name of the variable
    integer(i4) :: grid = nogrid               !< ID of the grid the data is defined on
    integer(i4) :: stepping = 0_i4             !< time-step size of this variable in hours (0 - static)
    logical :: static = .false.                !< flag to indicated static data (.false. by default)
    logical :: provided = .false.              !< flag to indicate that data is provided by a component (.false. by default)
    logical :: required = .false.              !< flag to indicate that data is required by a component (.false. by default)
  end type variable_abc

  !> \class   var_dp
  !> \brief   Class for a double precision variable in the exchange type.
  type, public, extends(variable_abc) :: var_dp
    real(dp), dimension(:), pointer :: data => null() !< 1D real pointer (n-cells)
  end type var_dp

  !> \class   var_i4
  !> \brief   Class for a 32bit integer variable in the exchange type.
  type, public, extends(variable_abc) :: var_i4
    integer(i4), dimension(:), pointer :: data => null() !< 1D integer pointer (n-cells)
  end type var_i4

  !> \class   var_lg
  !> \brief   Class for a logical variable in the exchange type.
  type, public, extends(variable_abc) :: var_lg
    logical, dimension(:), pointer :: data => null() !< 1D logical pointer (n-cells)
  end type var_lg

  !> \class   var2d_dp
  !> \brief   Class for a double precision variable for each horizon in the exchange type.
  type, public, extends(variable_abc) :: var2d_dp
    real(dp), dimension(:,:), pointer :: data => null() !< 2D real pointer (n-cells, horizons)
  end type var2d_dp

  !> \class   var2d_i4
  !> \brief   Class for a 32bit integer variable for each horizon in the exchange type.
  type, public, extends(variable_abc) :: var2d_i4
    integer(i4), dimension(:,:), pointer :: data => null() !< 2D integer pointer (n-cells, horizons)
  end type var2d_i4

  !> \class   var2d_lg
  !> \brief   Class for a logical variable for each horizon in the exchange type.
  type, public, extends(variable_abc) :: var2d_lg
    logical, dimension(:,:), pointer :: data => null() !< 2D logical pointer (n-cells, horizons)
  end type var2d_lg

  !> \class   exchange_t
  !> \brief   Class for dynamically exchanging variables in mHM.
  type, public :: exchange_t
    integer(i4) :: step_count !< current time step
    type(datetime) :: time    !< time-stamp for the current time step
    type(datetime) :: start_time    !< start time of simulation
    type(datetime) :: end_time    !< end time of simulation
    type(timedelta) :: step   !< time step of the simulation
    type(parameter_config_t) :: parameter_config !< parameter configuration
    type(process_config_t) :: process_config !< process configuration
    type(time_config_t) :: time_config !< time configuration
    !> Info about which process runs in which option and number of parameters necessary for this option
    !! - col1: process_switch
    !! - col2: no. of parameters
    !! - col3: cum. no. of parameters
    integer(i4), dimension(nprocesses, 3) :: process_matrix
    !> Matrix of global parameters (former: gamma)
    !! - col1: min
    !! - col2: max
    !! - col3: initial
    !! - col4: flag
    !! - col5: scaling
    real(dp), dimension(:, :), allocatable :: parameters_definition
    !> Array of global parameters names
    character(256), dimension(:), allocatable :: parameters_name
    !> Array of current parameter set
    real(dp), dimension(:), allocatable :: parameters
    integer(i4) :: nGeoUnits !< Number of geological formations
    integer(i4) :: domain !< Number of this domain

    ! grids
    type(grid_t), pointer :: level0  => null() !< level0 grid of the morphology
    type(grid_t), pointer :: level1  => null() !< level1 grid of the hydrology
    type(grid_t), pointer :: level2  => null() !< level2 grid of the meteorology
    type(grid_t), pointer :: level11 => null() !< level11 grid of the river network

    ! variables
    ! raw meteorology (level2)
    type(var_dp) :: raw_pre             !< raw precipitation [mm] on level l2
    type(var_dp) :: raw_temp            !< raw air temperature [degC] on level l2
    type(var_dp) :: raw_ssrd            !< raw solar short wave radiation downward [W m-2] on level l2
    type(var_dp) :: raw_strd            !< raw surface thermal radiation downward [W m-2] on level l2
    type(var_dp) :: raw_tann            !< raw annual mean air temperature [degC] on level l2
    type(var_dp) :: raw_tmin            !< raw minimum daily temperature [degC] on level l2
    type(var_dp) :: raw_tmax            !< raw maximum daily temperature [degC] on level l2
    type(var_dp) :: raw_netrad          !< raw net radiation [W m-2] on level l2
    type(var_dp) :: raw_eabs            !< raw vapor pressure [Pa] on level l2
    type(var_dp) :: raw_wind            !< raw wind speed [m s-1] on level l2

    ! processed meteorology (level1)
    type(var_dp) :: pre                 !< precipitation [mm] on level l1
    type(var_dp) :: temp                !< air temperature [degC] on level l1
    type(var_dp) :: pet                 !< potential evapotranspiration [mm] on level l1
    type(var_dp) :: ssrd                !< solar short wave radiation downward [W m-2] on level l1
    type(var_dp) :: strd                !< surface thermal radiation downward [W m-2] on level l1
    type(var_dp) :: tann                !< annual mean air temperature [degC] on level l1

    ! morphology (level0)
    type(var_dp) :: dem                 !< elevation [m] on level l0 (static)
    type(var_dp) :: slope               !< slope [%] on level l0 (static)
    type(var_dp) :: aspect              !< aspect [degree] on level l0 (static)

    ! hydrology (level1)
    ! canopy
    type(var_dp) :: interception        !< canopy interception storage [mm] on level l1
    type(var_dp) :: throughfall         !< throughfall amount [mm] on level l1
    ! storage and SM
    type(var2d_dp) :: soil_moisture     !< soil water content of soil layer [mm] on level l1
    type(var_dp) :: sealed_storage      !< reservoir of sealed areas [mm] on level l1
    type(var_dp) :: unsat_storage       !< reservoir of unsaturated zone [mm] on level l1
    type(var_dp) :: sat_storage         !< water level in groundwater reservoir [mm] on level l1
    ! type(var_dp) :: water_table_depth   !< depth to water table in groundwater reservoir [m] on level l1
    ! AET
    type(var_dp) :: aet_canopy          !< actual evapotranspiration from canopy [mm] on level l1
    type(var_dp) :: aet_sealed          !< actual evapotranspiration from free water surfaces [mm] on level l1
    type(var2d_dp) :: aet_soil          !< actual evapotranspiration from soil layer [mm] on level l1
    ! rain/snow
    type(var_dp) :: snowpack            !< depth of snowpack [mm] on level l1
    type(var_dp) :: rain                !< rain precipitation [mm] on level l1
    type(var_dp) :: snow                !< snow precipitation [mm] on level l1
    type(var_dp) :: melt                !< melting snow [mm] on level l1
    type(var_dp) :: pre_eff             !< effective precipitation [mm] on level l1 (rain + melt)
    ! vertical soil water movement
    type(var2d_dp) :: infiltration      !< infiltration intensity in soil layer [mm] on level l1
    type(var_dp) :: percolation         !< percolation [mm] on level l1
    ! type(var_dp) :: loss                !< gain/loss flux in a leaking linear reservoir [mm] on level l1
    ! lateral water movement
    type(var_dp) :: runoff_total        !< total runoff [mm] on level l1
    type(var_dp) :: runoff_sealed       !< direct runoff from impervious areas [mm] on level l1
    type(var_dp) :: interflow_fast      !< fast runoff component [mm] on level l1
    type(var_dp) :: interflow_slow      !< slow runoff component [mm] on level l1
    type(var_dp) :: baseflow            !< baseflow [mm] on level l1
    ! neutrons
    type(var_dp) :: neutrons            !< ground albedo neutrons [count h-1] on level l1
    ! degday calculated by mHM from MPR degday_X variables
    type(var_dp) :: degday              !< Degree-day factor [mm TS-1 degC-1] on level l1

    ! MPR results (level1)
    ! PET
    type(var_dp) :: pet_coeff_pt        !< PET calculation coefficient for Priestley Taylor (alpha) [1] on level l1
    type(var_dp) :: pet_coeff_hs        !< PET calculation coefficient for Hargreaves Samani [1] on level l1
    type(var_dp) :: pet_fac_aspect      !< PET correction based on aspect [1] on level l1
    type(var_dp) :: pet_fac_lai         !< PET correction based on LAI [1] on level l1
    type(var_dp) :: resist_aero         !< aerodynamical resistance [s m-1] on level l1
    type(var_dp) :: resist_surf         !< bulk surface resistance [s m-1] on level l1
    ! canopy
    type(var_dp) :: max_interception    !< Maximum interception [mm] on level l1
    ! snow
    type(var_dp) :: degday_inc          !< Increase of the degree-day factor per precipitation [TS-1 degC-1] on level l1
    type(var_dp) :: degday_max          !< Maximum degree-day factor [mm TS-1 degC-1] on level l1
    type(var_dp) :: degday_dry          !< Degree-day factor for no precipitation [mm TS-1 degC-1] on level l1
    type(var_dp) :: thresh_temp         !< Threshold temperature for phase transition snow and rain [degC] on level l1
    ! soil moisture
    type(var_dp) :: f_sealed            !< Fraction of sealed area [1] on level l1
    type(var2d_dp) :: f_roots           !< Fraction of roots in soil horizons [1] on level l1
    type(var2d_dp) :: sm_saturation     !< Saturation soil moisture [mm] on level l1
    type(var2d_dp) :: sm_exponent       !< Exponential parameter controlling non-linearity of soil water retention [1] on level l1
    type(var2d_dp) :: sm_field_capacity !< Field capacity - soil moisture below which actual ET is reduced [mm] on level l1
    type(var2d_dp) :: wilting_point     !< permanent wilting point [mm] on level l1
    type(var_dp) :: thresh_jarvis       !< Jarvis critical value (C1) for normalized soil water content [1] on level l1
    ! runoff
    type(var_dp) :: alpha               !< Exponent for the upper reservoir [1] on level l1
    type(var_dp) :: k_fastflow          !< Fast interflow recession coefficient [TS-1] on level l1
    type(var_dp) :: k_slowflow          !< Slow interflow recession coefficient [TS-1] on level l1
    type(var_dp) :: k_baseflow          !< Baseflow recession coefficient [TS-1] on level l1
    type(var_dp) :: k_percolation       !< Percolation coefficient [TS-1] on level l1
    type(var_dp) :: f_karst_loss        !< Fraction of karstic percolation loss [1] on level l1
    type(var_dp) :: thresh_unsat        !< Threshold water depth for fast interflow [mm] on level l1
    type(var_dp) :: thresh_sealed       !< Threshold water depth for runoff on sealed surfaces [mm] on level l1
    ! neutrons
    type(var_dp) :: desilets_n0         !< neutron count rate under dry reference conditions (N_0 in Desilets eq.) [count h-1] on level l1
    type(var2d_dp) :: bulk_density      !< bulk density [g cm-3] on level l1
    type(var2d_dp) :: lattice_water     !< Ratio of structurally bound water [g g-1] on level l1
    type(var2d_dp) :: cosmic_l3         !< cosmic L3 parameter [g cm-2] on level l1

    ! routing (level11)
    type(var_dp) :: q_out               !< accumulated runoff [m3 s-1] on level l11
    type(var_dp) :: q_mod               !< modelled discharge [m3 s-1] on level l11
    type(var_dp) :: e_out               !< accumulated source energy [W] on level l11
    type(var_dp) :: e_mod               !< modelled routed energy [W] on level l11
    type(var_dp) :: river_temp          !< simulated river temperature [degC] on level l11

    ! groundwater (level0)
    type(var_dp) :: riverhead           !< simulated riverhead [m] on level l0

  contains
    procedure, public  :: init => exchange_init
    procedure, public  :: get_grid => exchange_get_grid
    procedure, public  :: has_grid => exchange_has_grid
    procedure, public :: get_meta => exchange_get_var_meta
    procedure, public :: config_parameter => exchange_config_parameter
    procedure, private :: get_var_class => exchange_get_var_class
    procedure, private  :: get_data_1d_dp => exchange_get_data_1d_dp
    procedure, private  :: get_data_1d_i4 => exchange_get_data_1d_i4
    procedure, private  :: get_data_1d_lg => exchange_get_data_1d_lg
    procedure, private  :: get_data_2d_dp => exchange_get_data_2d_dp
    procedure, private  :: get_data_2d_i4 => exchange_get_data_2d_i4
    procedure, private  :: get_data_2d_lg => exchange_get_data_2d_lg
    generic, public :: get_data => get_data_1d_dp, get_data_1d_i4, get_data_1d_lg, get_data_2d_dp, get_data_2d_i4, get_data_2d_lg
    procedure, private  :: set_data_1d => exchange_set_data_1d
    procedure, private  :: set_data_2d => exchange_set_data_2d
    generic, public :: set_data => set_data_1d, set_data_2d
  end type exchange_t

contains

  !> \brief initialize the exchange type
  subroutine exchange_init(self, time_cfg, parameter_cfg, process_cfg)
    class(exchange_t), intent(inout) :: self
    type(time_config_t), intent(in) :: time_cfg !< time configuration
    type(parameter_config_t), intent(in) :: parameter_cfg !< parameter_configuration
    type(process_config_t), intent(in) :: process_cfg !< process configuration

    call message(" ... init exchange")
    self%time_config = time_cfg
    self%parameter_config = parameter_cfg
    self%process_config = process_cfg
    self%domain = self%time_config%domain

    ! time settings
    self%step_count = 0_i4
    self%start_time = datetime( &
      year=self%time_config%time_periods%eval_Per(self%domain)%yStart, &
      month=self%time_config%time_periods%eval_Per(self%domain)%mStart, &
      day=self%time_config%time_periods%eval_Per(self%domain)%dStart &
    )
    self%end_time = datetime( &
      year=self%time_config%time_periods%eval_Per(self%domain)%yEnd, &
      month=self%time_config%time_periods%eval_Per(self%domain)%mEnd, &
      day=self%time_config%time_periods%eval_Per(self%domain)%dEnd &
    )
    self%start_time = self%start_time - timedelta(days=self%time_config%time_periods%warming_Days(self%domain))
    self%time = self%start_time
    self%step = timedelta(hours=self%time_config%mainconfig_mhm_mrm%timeStep)

    ! prepare parameters and process_matrix
    call self%config_parameter()

    ! variables
    ! raw meteorology (level2)
    self%raw_pre    = var_dp(grid=l2, name="pre",       units="mm",    long_name="precipitation", standard_name="precipitation_amount")
    self%raw_temp   = var_dp(grid=l2, name="temp",      units="degC",  long_name="air temperature", standard_name="air_temperature")
    self%raw_ssrd   = var_dp(grid=l2, name="ssrd",      units="W m-2", long_name="solar short wave radiation downward", standard_name="surface_downwelling_shortwave_flux")
    self%raw_strd   = var_dp(grid=l2, name="strd",      units="W m-2", long_name="surface thermal radiation downward", standard_name="surface_downwelling_longwave_flux")
    self%raw_tann   = var_dp(grid=l2, name="tann",      units="degC",  long_name="annual mean air temperature", standard_name="air_temperature")
    self%raw_tmin   = var_dp(grid=l2, name="tmin",      units="degC",  long_name="minimum daily temperature", standard_name="air_temperature")
    self%raw_tmax   = var_dp(grid=l2, name="tmax",      units="degC",  long_name="maximum daily temperature", standard_name="air_temperature")
    self%raw_netrad = var_dp(grid=l2, name="netrad",    units="W m-2", long_name="net radiation", standard_name="surface_net_downward_radiative_flux")
    self%raw_eabs   = var_dp(grid=l2, name="eabs",      units="Pa",    long_name="vapor pressure", standard_name="water_vapor_pressure")
    self%raw_wind   = var_dp(grid=l2, name="windspeed", units="m s-1", long_name="wind speed", standard_name="wind_speed")

    ! processed meteorology (level1)
    self%pre  = var_dp(grid=l1, name="pre",  units="mm",    long_name="precipitation", standard_name="precipitation_amount")
    self%temp = var_dp(grid=l1, name="temp", units="degC",  long_name="air temperature", standard_name="air_temperature")
    self%pet  = var_dp(grid=l1, name="pet",  units="mm",    long_name="potential evapotranspiration", standard_name="water_potential_evapotranspiration_amount")
    self%ssrd = var_dp(grid=l1, name="ssrd", units="W m-2", long_name="solar short wave radiation downward", standard_name="surface_downwelling_shortwave_flux")
    self%strd = var_dp(grid=l1, name="strd", units="W m-2", long_name="surface thermal radiation downward", standard_name="surface_downwelling_longwave_flux")
    self%tann = var_dp(grid=l1, name="tann", units="degC",  long_name="annual mean air temperature", standard_name="air_temperature")

    ! morphology (level0)
    self%dem    = var_dp(static=.true., grid=l0, name="dem",    units="m",      long_name="elevation", standard_name="height_above_mean_sea_level")
    self%slope  = var_dp(static=.true., grid=l0, name="slope",  units="%",      long_name="slope", standard_name="ground_slope_angle")
    self%aspect = var_dp(static=.true., grid=l0, name="aspect", units="degree", long_name="aspect", standard_name="ground_slope_direction")

    ! hydrology (level1)
    ! canopy
    self%interception      =   var_dp(grid=l1, name="interception",      units="mm",  long_name="canopy interception storage")
    self%throughfall       =   var_dp(grid=l1, name="throughfall",       units="mm",  long_name="throughfall amount")
    ! storage and SM
    self%soil_moisture     = var2d_dp(grid=l1, name="soil_moisture",     units="mm",  long_name="soil water content of soil layer")
    self%sealed_storage    =   var_dp(grid=l1, name="sealedSTW",         units="mm",  long_name="reservoir of sealed areas")
    self%unsat_storage     =   var_dp(grid=l1, name="unsatSTW",          units="mm",  long_name="reservoir of unsaturated zone")
    self%sat_storage       =   var_dp(grid=l1, name="satSTW",            units="mm",  long_name="water level in groundwater reservoir")
    ! AET
    self%aet_canopy        =   var_dp(grid=l1, name="aet_canopy",        units="mm",  long_name="actual evapotranspiration from canopy")
    self%aet_sealed        =   var_dp(grid=l1, name="aet_sealed",        units="mm",  long_name="actual evapotranspiration from free water surfaces")
    self%aet_soil          = var2d_dp(grid=l1, name="aet_soil",          units="mm",  long_name="actual evapotranspiration from soil layer")
    ! rain/snow
    self%snowpack          =   var_dp(grid=l1, name="snowpack",          units="mm",  long_name="depth of snowpack", standard_name="surface_snow_amount")
    self%rain              =   var_dp(grid=l1, name="rain",              units="mm",  long_name="rain precipitation", standard_name="rainfall_amount")
    self%snow              =   var_dp(grid=l1, name="snow",              units="mm",  long_name="snow precipitation", standard_name="snowfall_amount")
    self%melt              =   var_dp(grid=l1, name="melt",              units="mm",  long_name="melting snow", standard_name="surface_snow_melt_amount")
    self%pre_eff           =   var_dp(grid=l1, name="pre_eff",           units="mm",  long_name="effective precipitation") ! rain + melt
    ! vertical soil water movement
    self%infiltration      = var2d_dp(grid=l1, name="infiltration",      units="mm",  long_name="infiltration intensity in soil layer")
    self%percolation       =   var_dp(grid=l1, name="percolation",       units="mm",  long_name="percolation")
    ! lateral water movement
    self%runoff_total      =   var_dp(grid=l1, name="Q",                 units="mm",  long_name="total runoff", standard_name="runoff_amount")
    self%runoff_sealed     =   var_dp(grid=l1, name="QD",                units="mm",  long_name="direct runoff from impervious areas", standard_name="surface_runoff_amount")
    self%interflow_fast    =   var_dp(grid=l1, name="QIf",               units="mm",  long_name="fast runoff component", standard_name="subsurface_runoff_amount")
    self%interflow_slow    =   var_dp(grid=l1, name="QIs",               units="mm",  long_name="slow runoff component", standard_name="subsurface_runoff_amount")
    self%baseflow          =   var_dp(grid=l1, name="QB",                units="mm",  long_name="baseflow", standard_name="baseflow_amount")
    ! neutrons
    self%neutrons          =   var_dp(grid=l1, name="neutrons",          units="cph", long_name="ground albedo neutrons")

    ! MPR results (level1)
    ! PET
    self%pet_coeff_pt      =   var_dp(grid=l1, name="pet_coeff_pt",      units="1",                 long_name="PET calculation coefficient for Priestley Taylor (alpha)")
    self%pet_coeff_hs      =   var_dp(grid=l1, name="pet_coeff_hs",      units="1", static=.true.,  long_name="PET calculation coefficient for Hargreaves Samani")
    self%pet_fac_aspect    =   var_dp(grid=l1, name="pet_fac_aspect",    units="1", static=.true.,  long_name="PET correction factor based on aspect")
    self%pet_fac_lai       =   var_dp(grid=l1, name="pet_fac_lai",       units="1",                 long_name="PET correction factor based on LAI")
    self%resist_aero       =   var_dp(grid=l1, name="resist_aero",       units="s m-1",             long_name="aerodynamical resistance")
    self%resist_surf       =   var_dp(grid=l1, name="resist_surf",       units="s m-1",             long_name="bulk surface resistance")
    ! canopy
    self%max_interception  =   var_dp(grid=l1, name="max_interception",  units="mm",                long_name="Maximum interception")
    ! snow
    self%degday_inc        =   var_dp(grid=l1, name="degday_inc",        units="TS-1 degC-1",       long_name="Increase of the degree-day factor per precipitation")
    self%degday_max        =   var_dp(grid=l1, name="degday_max",        units="mm TS-1 degC-1",    long_name="Maximum degree-day factor")
    self%degday_dry        =   var_dp(grid=l1, name="degday_dry",        units="mm TS-1 degC-1",    long_name="Degree-day factor for no precipitation")
    self%thresh_temp       =   var_dp(grid=l1, name="thresh_temp",       units="degC",              long_name="Threshold temperature for phase transition snow and rain")
    ! soil moisture
    self%f_sealed          =   var_dp(grid=l1, name="f_sealed",          units="1",                 long_name="Fraction of sealed area")
    self%f_roots           = var2d_dp(grid=l1, name="f_roots",           units="1",                 long_name="Fraction of roots in soil horizons")
    self%sm_saturation     = var2d_dp(grid=l1, name="sm_saturation",     units="mm",                long_name="Saturation soil moisture")
    self%sm_exponent       = var2d_dp(grid=l1, name="sm_exponent",       units="1",                 long_name="Exponential parameter controlling non-linearity of soil water retention")
    self%sm_field_capacity = var2d_dp(grid=l1, name="sm_field_capacity", units="mm",                long_name="Field capacity - soil moisture below which actual ET is reduced")
    self%wilting_point     = var2d_dp(grid=l1, name="wilting_point",     units="mm",                long_name="permanent wilting point")
    self%thresh_jarvis     =   var_dp(grid=l1, name="thresh_jarvis",     units="1",  static=.true., long_name="Jarvis critical value (C1) for normalized soil water content")
    ! runoff
    self%alpha             =   var_dp(grid=l1, name="alpha",             units="1",                 long_name="Exponent for the upper reservoir")
    self%k_fastflow        =   var_dp(grid=l1, name="k_fastflow",        units="TS-1",              long_name="Fast interflow recession coefficient")
    self%k_slowflow        =   var_dp(grid=l1, name="k_slowflow",        units="TS-1",              long_name="Slow interflow recession coefficient")
    self%k_baseflow        =   var_dp(grid=l1, name="k_baseflow",        units="TS-1",              long_name="Baseflow recession coefficient")
    self%k_percolation     =   var_dp(grid=l1, name="k_percolation",     units="TS-1",              long_name="Percolation coefficient")
    self%f_karst_loss      =   var_dp(grid=l1, name="f_karst_loss",      units="1",  static=.true., long_name="Fraction of karstic percolation loss")
    self%thresh_unsat      =   var_dp(grid=l1, name="thresh_unsat",      units="mm", static=.true., long_name="Threshold water depth for fast interflow")
    self%thresh_sealed     =   var_dp(grid=l1, name="thresh_sealed",     units="mm", static=.true., long_name="Threshold water depth for runoff on sealed surfaces")
    ! neutrons
    self%desilets_n0       =   var_dp(grid=l1, name="desilets_n0",       units="count h-1", static=.true., long_name="neutron count rate under dry reference conditions (N_0 in Desilets eq.)")
    self%bulk_density      = var2d_dp(grid=l1, name="bulk_density",      units="g cm-3",            long_name="bulk density")
    self%lattice_water     = var2d_dp(grid=l1, name="lattice_water",     units="g g-1",             long_name="Ratio of structurally bound water")
    self%cosmic_l3         = var2d_dp(grid=l1, name="cosmic_l3",         units="g cm-2",            long_name="cosmic L3 parameter")

    ! routing (level11)
    self%q_out             =   var_dp(grid=l11, name="q_out",            units="m3 s-1",            long_name="accumulated runoff")
    self%q_mod             =   var_dp(grid=l11, name="q_mod",            units="m3 s-1",            long_name="modelled discharge")
    self%e_out             =   var_dp(grid=l11, name="e_out",            units="W",                 long_name="accumulated source energy")
    self%e_mod             =   var_dp(grid=l11, name="e_mod",            units="W",                 long_name="modelled routed energy")
    self%river_temp        =   var_dp(grid=l11, name="river_temp",       units="degC",              long_name="simulated river temperature")

    ! groundwater (level0)
    self%riverhead         =   var_dp(grid=l0,  name="riverhead",        units="m",                 long_name="simulated riverhead")
  end subroutine exchange_init

  !> \brief get the grid specifications for the selected level
  subroutine exchange_get_grid(self, selector, grid)
    use mo_message, only: error_message
    class(exchange_t), intent(in) :: self
    integer(i4), intent(in) :: selector !< level selector (0: l0, 1: l1, 2: l2, 3: l11, -1: nogrid)
    type(grid_t), pointer, intent(out) :: grid !< resulting pointer to the selected grid
    select case(selector)
      case(nogrid)
        grid => null() ! exchangable
      case(l0)
        grid => self%level0
      case(l1)
        grid => self%level1
      case(l2)
        grid => self%level2
      case(l11)
        grid => self%level11
      case default
        call error_message("exchange%get_grid: unknown grid selector '", n2s(selector), "'.")
    end select
  end subroutine exchange_get_grid

  !> \brief get the grid specifications for the selected level
  logical function exchange_has_grid(self, selector)
    use mo_message, only: error_message
    class(exchange_t), intent(in) :: self
    integer(i4), intent(in) :: selector !< level selector (0: l0, 1: l1, 2: l2, 3: l11, -1: nogrid)
    select case(selector)
      case(l0)
        exchange_has_grid = associated(self%level0)
      case(l1)
        exchange_has_grid = associated(self%level1)
      case(l2)
        exchange_has_grid = associated(self%level2)
      case(l11)
        exchange_has_grid = associated(self%level11)
      case default
        exchange_has_grid = .false.
    end select
  end function exchange_has_grid

  !> \brief get class pointer to a variable
  subroutine exchange_get_var_class(self, var, var_pnt)
    use mo_message, only: error_message
    class(exchange_t), target, intent(in) :: self ! target attribute valid here since Fortran 2003
    character(*), intent(in) :: var !< name of the variable (attribute name)
    class(*), pointer, intent(out) :: var_pnt !< resulting pointer to the selected variable
    select case(var)
      case("raw_pre")
        var_pnt => self%raw_pre
      case("raw_temp")
        var_pnt => self%raw_temp
      case("raw_ssrd")
        var_pnt => self%raw_ssrd
      case("raw_strd")
        var_pnt => self%raw_strd
      case("raw_tann")
        var_pnt => self%raw_tann
      case("raw_tmin")
        var_pnt => self%raw_tmin
      case("raw_tmax")
        var_pnt => self%raw_tmax
      case("raw_netrad")
        var_pnt => self%raw_netrad
      case("raw_eabs")
        var_pnt => self%raw_eabs
      case("raw_wind")
        var_pnt => self%raw_wind
      ! processed meteorology (level1)
      case("pre")
        var_pnt => self%pre
      case("temp")
        var_pnt => self%temp
      case("pet")
        var_pnt => self%pet
      case("ssrd")
        var_pnt => self%ssrd
      case("strd")
        var_pnt => self%strd
      case("tann")
        var_pnt => self%tann
      ! morphology (level0)
      case("dem")
        var_pnt => self%dem
      case("slope")
        var_pnt => self%slope
      case("aspect")
        var_pnt => self%aspect
      ! hydrology (level1)
      ! canopy
      case("interception")
        var_pnt => self%interception
      case("throughfall")
        var_pnt => self%throughfall
      ! storage and SM
      case("soil_moisture")
        var_pnt => self%soil_moisture
      case("sealed_storage")
        var_pnt => self%sealed_storage
      case("unsat_storage")
        var_pnt => self%unsat_storage
      case("sat_storage")
        var_pnt => self%sat_storage
      ! case("water_table_depth")
      !   var => self%water_table_depth
      ! AET
      case("aet_canopy")
        var_pnt => self%aet_canopy
      case("aet_sealed")
        var_pnt => self%aet_sealed
      case("aet_soil")
        var_pnt => self%aet_soil
      ! rain/snow
      case("snowpack")
        var_pnt => self%snowpack
      case("rain")
        var_pnt => self%rain
      case("snow")
        var_pnt => self%snow
      case("melt")
        var_pnt => self%melt
      case("pre_eff")
        var_pnt => self%pre_eff
      ! vertical soil water movement
      case("infiltration")
        var_pnt => self%infiltration
      case("percolation")
        var_pnt => self%percolation
      ! case("loss")
      !   var => self%loss
      ! lateral water movement
      case("runoff_total")
        var_pnt => self%runoff_total
      case("runoff_sealed")
        var_pnt => self%runoff_sealed
      case("interflow_fast")
        var_pnt => self%interflow_fast
      case("interflow_slow")
        var_pnt => self%interflow_slow
      case("baseflow")
        var_pnt => self%baseflow
      ! neutrons
      case("neutrons")
        var_pnt => self%neutrons
      ! degday calculated by mHM from MPR degday_X variables
      case("degday")
        var_pnt => self%degday
      ! MPR results (level1)
      ! PET
      case("pet_coeff_pt")
        var_pnt => self%pet_coeff_pt
      case("pet_coeff_hs")
        var_pnt => self%pet_coeff_hs
      case("pet_fac_aspect")
        var_pnt => self%pet_fac_aspect
      case("pet_fac_lai")
        var_pnt => self%pet_fac_lai
      case("resist_aero")
        var_pnt => self%resist_aero
      case("resist_surf")
        var_pnt => self%resist_surf
      ! canopy
      case("max_interception")
        var_pnt => self%max_interception
      ! snow
      case("degday_inc")
        var_pnt => self%degday_inc
      case("degday_max")
        var_pnt => self%degday_max
      case("degday_dry")
        var_pnt => self%degday_dry
      case("thresh_temp")
        var_pnt => self%thresh_temp
      ! soil moisture
      case("f_sealed")
        var_pnt => self%f_sealed
      case("f_roots")
        var_pnt => self%f_roots
      case("sm_saturation")
        var_pnt => self%sm_saturation
      case("sm_exponent")
        var_pnt => self%sm_exponent
      case("sm_field_capacity")
        var_pnt => self%sm_field_capacity
      case("wilting_point")
        var_pnt => self%wilting_point
      case("thresh_jarvis")
        var_pnt => self%thresh_jarvis
      ! runoff
      case("alpha")
        var_pnt => self%alpha
      case("k_fastflow")
        var_pnt => self%k_fastflow
      case("k_slowflow")
        var_pnt => self%k_slowflow
      case("k_baseflow")
        var_pnt => self%k_baseflow
      case("k_percolation")
        var_pnt => self%k_percolation
      case("f_karst_loss")
        var_pnt => self%f_karst_loss
      case("thresh_unsat")
        var_pnt => self%thresh_unsat
      case("thresh_sealed")
        var_pnt => self%thresh_sealed
      ! neutrons
      case("desilets_n0")
        var_pnt => self%desilets_n0
      case("bulk_density")
        var_pnt => self%bulk_density
      case("lattice_water")
        var_pnt => self%lattice_water
      case("cosmic_l3")
        var_pnt => self%cosmic_l3
      ! routing (level11)
      case("q_out")
        var_pnt => self%q_out
      case("q_mod")
        var_pnt => self%q_mod
      case("e_out")
        var_pnt => self%e_out
      case("e_mod")
        var_pnt => self%e_mod
      case("river_temp")
        var_pnt => self%river_temp
      ! groundwater (level0)
      case("riverhead")
        var_pnt => self%riverhead
      case default
        call error_message("exchange%get_var: variable '", var, "' not available.")
    end select
  end subroutine exchange_get_var_class

  !> \brief get var_dp pointer to a variable
  subroutine exchange_get_var_meta(self, var, name, units, long_name, standard_name, grid, static, provided, required)
    use mo_message, only: error_message
    class(exchange_t), target, intent(in) :: self ! target attribute valid here since Fortran 2003
    character(*), intent(in) :: var                                   !< name of the variable (attribute name)
    character(:), allocatable, intent(out), optional :: name          !< variable name
    character(:), allocatable, intent(out), optional :: units         !< variable unit
    character(:), allocatable, intent(out), optional :: long_name     !< long name of the variable
    character(:), allocatable, intent(out), optional :: standard_name !< standard name of the variable
    integer(i4), intent(out), optional :: grid                        !< ID of the grid the data is defined on
    logical, intent(out), optional :: static                          !< flag to indicated static data
    logical, intent(out), optional :: provided                        !< flag to indicate that data is provided by a component
    logical, intent(out), optional :: required                        !< flag to indicate that data is required by a component
    class(*), pointer :: tmp
    call self%get_var_class(var, tmp)
    select type (tmp)
      class is (variable_abc)
        if (present(name)          .and. allocated(tmp%name))          name          = tmp%name
        if (present(units)         .and. allocated(tmp%units))         units         = tmp%units
        if (present(long_name)     .and. allocated(tmp%long_name))     long_name     = tmp%long_name
        if (present(standard_name) .and. allocated(tmp%standard_name)) standard_name = tmp%standard_name
        if (present(grid))     grid     = tmp%grid
        if (present(static))   static   = tmp%static
        if (present(provided)) provided = tmp%provided
        if (present(required)) required = tmp%required
    end select
  end subroutine exchange_get_var_meta

  !> \brief get pointer to the 1D variable data
  subroutine exchange_get_data_1d_dp(self, var, data)
    use mo_message, only: error_message
    class(exchange_t), target, intent(in) :: self ! target attribute valid here since Fortran 2003
    character(*), intent(in) :: var !< name of the variable (attribute name)
    real(dp), pointer, intent(out) :: data(:) !< resulting pointer to the selected variable data
    class(*), pointer :: tmp
    call self%get_var_class(var, tmp)
    select type (tmp)
      class is (var_dp)
        data => tmp%data
      class default
        call error_message("exchange%get_var: variable data of '", var, "' not 1D real(dp).")
    end select
  end subroutine exchange_get_data_1d_dp

  !> \brief get pointer to the 1D variable data
  subroutine exchange_get_data_1d_i4(self, var, data)
    use mo_message, only: error_message
    class(exchange_t), target, intent(in) :: self ! target attribute valid here since Fortran 2003
    character(*), intent(in) :: var !< name of the variable (attribute name)
    integer(i4), pointer, intent(out) :: data(:) !< resulting pointer to the selected variable data
    class(*), pointer :: tmp
    call self%get_var_class(var, tmp)
    select type (tmp)
      class is (var_i4)
        data => tmp%data
      class default
        call error_message("exchange%get_var: variable data of '", var, "' not 1D integer(i4).")
    end select
  end subroutine exchange_get_data_1d_i4

  !> \brief get pointer to the 1D variable data
  subroutine exchange_get_data_1d_lg(self, var, data)
    use mo_message, only: error_message
    class(exchange_t), target, intent(in) :: self ! target attribute valid here since Fortran 2003
    character(*), intent(in) :: var !< name of the variable (attribute name)
    logical, pointer, intent(out) :: data(:) !< resulting pointer to the selected variable data
    class(*), pointer :: tmp
    call self%get_var_class(var, tmp)
    select type (tmp)
      class is (var_lg)
        data => tmp%data
      class default
        call error_message("exchange%get_var: variable data of '", var, "' not 1D logical.")
    end select
  end subroutine exchange_get_data_1d_lg

  !> \brief get pointer to the 2D variable data
  subroutine exchange_get_data_2d_dp(self, var, data)
  use mo_message, only: error_message
    class(exchange_t), target, intent(in) :: self ! target attribute valid here since Fortran 2003
    character(*), intent(in) :: var !< name of the variable (attribute name)
    real(dp), pointer, intent(out) :: data(:,:) !< resulting pointer to the selected variable data
    class(*), pointer :: tmp
    call self%get_var_class(var, tmp)
    select type (tmp)
      class is (var2d_dp)
        data => tmp%data
      class default
        call error_message("exchange%get_var: variable data of '", var, "' not 2D real(dp).")
    end select
  end subroutine exchange_get_data_2d_dp

  !> \brief get pointer to the 2D variable data
  subroutine exchange_get_data_2d_i4(self, var, data)
    use mo_message, only: error_message
    class(exchange_t), target, intent(in) :: self ! target attribute valid here since Fortran 2003
    character(*), intent(in) :: var !< name of the variable (attribute name)
    integer(i4), pointer, intent(out) :: data(:,:) !< resulting pointer to the selected variable data
    class(*), pointer :: tmp
    call self%get_var_class(var, tmp)
    select type (tmp)
      class is (var2d_i4)
        data => tmp%data
      class default
        call error_message("exchange%get_var: variable data of '", var, "' not 2D integer(i4).")
    end select
  end subroutine exchange_get_data_2d_i4

  !> \brief get pointer to the 2D variable data
  subroutine exchange_get_data_2d_lg(self, var, data)
    use mo_message, only: error_message
    class(exchange_t), target, intent(in) :: self ! target attribute valid here since Fortran 2003
    character(*), intent(in) :: var !< name of the variable (attribute name)
    logical, pointer, intent(out) :: data(:,:) !< resulting pointer to the selected variable data
    class(*), pointer :: tmp
    call self%get_var_class(var, tmp)
    select type (tmp)
      class is (var2d_lg)
        data => tmp%data
      class default
        call error_message("exchange%get_var: variable data of '", var, "' not 2D logical.")
    end select
  end subroutine exchange_get_data_2d_lg

  !> \brief set pointer to the 1D variable data
  subroutine exchange_set_data_1d(self, var, data)
    use mo_message, only: error_message
    class(exchange_t), target, intent(inout) :: self ! target attribute valid here since Fortran 2003
    character(*), intent(in) :: var !< name of the variable (attribute name)
    class(*), target, intent(in) :: data(:) !< target data
    class(*), pointer :: tmp
    call self%get_var_class(var, tmp)
    select type (tmp)
      class is (var_dp)
        select type (data)
          type is (real(dp))
            tmp%data => data
          class default
            call error_message("exchange%get_var: variable data of '", var, "' is of type real(dp).")
        end select
      class is (var_i4)
        select type (data)
          type is (integer(i4))
            tmp%data => data
          class default
            call error_message("exchange%get_var: variable data of '", var, "' is of type integer(i4).")
        end select
      class is (var_lg)
        select type (data)
          type is (logical)
            tmp%data => data
          class default
            call error_message("exchange%get_var: variable data of '", var, "' is of type logical.")
        end select
      class default
        call error_message("exchange%get_var: variable data of '", var, "' not one dimensional.")
    end select
  end subroutine exchange_set_data_1d

  !> \brief set target for variable data 2D
  subroutine exchange_set_data_2d(self, var, data)
    use mo_message, only: error_message
    class(exchange_t), target, intent(inout) :: self ! target attribute valid here since Fortran 2003
    character(*), intent(in) :: var !< name of the variable (attribute name)
    class(*), target, intent(in) :: data(:,:) !< target data
    class(*), pointer :: tmp
    call self%get_var_class(var, tmp)
    select type (tmp)
      class is (var2d_dp)
        select type (data)
          type is (real(dp))
            tmp%data => data
          class default
            call error_message("exchange%get_var: variable data of '", var, "' is of type real(dp).")
        end select
      class is (var2d_i4)
        select type (data)
          type is (integer(i4))
            tmp%data => data
          class default
            call error_message("exchange%get_var: variable data of '", var, "' is of type integer(i4).")
        end select
      class is (var2d_lg)
        select type (data)
          type is (logical)
            tmp%data => data
          class default
            call error_message("exchange%get_var: variable data of '", var, "' is of type logical.")
        end select
      class default
        call error_message("exchange%get_var: variable data of '", var, "' not two dimensional.")
    end select
  end subroutine exchange_set_data_2d

  !> \brief Read time configuration.
  subroutine time_config_read(self, file, domain)
    class(time_config_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelists
    integer(i4), intent(in) :: domain !< domain number to read correct configuration
    call message(" ... config time: ", file)
    self%file = file
    self%domain = domain
    call self%mainconfig_mhm_mrm%read(file)
    call self%time_periods%read(file)
  end subroutine time_config_read

  !> \brief Read parameter configuration.
  subroutine parameter_config_read(self, file)
    class(parameter_config_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelists
    call message(" ... config parameter: ", file)
    ! all parameter namelists have a "status" attribute to indicate if the namelist was present
    self%file = file
    call self%interception1%read(file)
    call self%snow1%read(file)
    call self%soilmoisture1%read(file)
    call self%soilmoisture2%read(file)
    call self%soilmoisture3%read(file)
    call self%soilmoisture4%read(file)
    call self%directrunoff1%read(file)
    call self%PETminus1%read(file)
    call self%PET0%read(file)
    call self%PET1%read(file)
    call self%PET2%read(file)
    call self%PET3%read(file)
    call self%interflow1%read(file)
    call self%percolation1%read(file)
    call self%routing1%read(file)
    call self%routing2%read(file)
    call self%routing3%read(file)
    call self%neutrons1%read(file)
    call self%neutrons2%read(file)
    call self%geoparameter%read(file)
  end subroutine parameter_config_read

  !> \brief Read process configuration.
  subroutine process_config_read(self, file)
    class(process_config_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelists
    call message(" ... config process: ", file)
    self%file = file
    call self%processselection%read(file)
  end subroutine process_config_read

  !>       \brief Configure parameters
  !>       \authors Stephan Thober
  !>       \date Aug 2015
  ! Modifications:
  ! Stephan Thober  Sep 2015 - removed stop condition when routing resolution is smaller than hydrologic resolution
  ! Stephan Thober  Oct 2015 - added NLoutputResults namelist, fileLatLon to directories_general namelist, and readLatLon flag
  ! Robert Schweppe Dec 2017 - adapted for MPR
  !  Rohini Kumar   Oct 2021 - Added Neutron count module to mHM integrate into develop branch (5.11.2)
  subroutine exchange_config_parameter(self)

    use mo_append, only : append
    use mo_common_constants, only : maxNoDomains, nColPars
    use mo_mpr_constants, only : maxGeoUnit
    use mo_constants, only : eps_dp, nodata_dp
    use mo_common_functions, only : in_bound
    use mo_message, only : message, error_message
    use mo_string_utils, only : num2str
    use mo_utils, only : EQ

    implicit none
    class(exchange_t), intent(inout) :: self

    character(256) :: dummy

    ! space holder for routing parameters
    real(dp), dimension(5, nColPars) :: dummy_2d_dp

    ! space holder for routing parameters
    real(dp), dimension(1, nColPars) :: dummy_2d_dp_2

    real(dp), dimension(maxGeoUnit, nColPars) :: GeoParam

    integer(i4) :: ii, shp(2)

    shp = [1_i4, ncolpars] ! shape of a parameter matrix slice

    !===============================================================
    ! Read namelist global parameters
    !===============================================================
    ! decide which parameters to read depending on specified processes

    call message(" ... configure processes and parameters")
    self%process_matrix = 0_i4
    self%process_matrix(:, 1) = self%process_config%processselection%processcase

    ! Process 1 - interception
    select case (self%process_matrix(1, 1))
      ! 1 - maximum Interception
      case(1)
        if (self%parameter_config%interception1%nml_status /= 0_i4) call error_message("'interception1' namelist not found.")

        self%process_matrix(1, 2) = 1_i4
        self%process_matrix(1, 3) = 1_i4
        call append(self%parameters_definition, reshape(self%parameter_config%interception1%canopyInterceptionFactor, shp))

        call append(self%parameters_name, [  &
                'canopyInterceptionFactor'])

        ! check if parameter are in range
        if (.not. in_bound(self%parameters_definition)) &
          call error_message('***ERROR: parameter in namelist "interception1" out of bound in ', self%parameter_config%file)

      case DEFAULT
        call error_message('***ERROR: Process description for process "interception" does not exist!')
        stop
    end select

    ! Process 2 - snow
    select case (self%process_matrix(2, 1))
      ! 1 - degree-day approach
      case(1)
        if (self%parameter_config%snow1%nml_status /= 0_i4) call error_message("'snow1' namelist not found.")

        self%process_matrix(2, 2) = 8_i4
        self%process_matrix(2, 3) = sum(self%process_matrix(1 : 2, 2))
        call append(self%parameters_definition, reshape(self%parameter_config%snow1%snowTreshholdTemperature, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%snow1%degreeDayFactor_forest, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%snow1%degreeDayFactor_impervious, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%snow1%degreeDayFactor_pervious, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%snow1%increaseDegreeDayFactorByPrecip, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%snow1%maxDegreeDayFactor_forest, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%snow1%maxDegreeDayFactor_impervious, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%snow1%maxDegreeDayFactor_pervious, shp))

        call append(self%parameters_name, [  &
                'snowTreshholdTemperature       ', &
                'degreeDayFactor_forest         ', &
                'degreeDayFactor_impervious     ', &
                'degreeDayFactor_pervious       ', &
                'increaseDegreeDayFactorByPrecip', &
                'maxDegreeDayFactor_forest      ', &
                'maxDegreeDayFactor_impervious  ', &
                'maxDegreeDayFactor_pervious    '])

        ! check if parameter are in range
        if (.not. in_bound(self%parameters_definition)) &
          call error_message('***ERROR: parameter in namelist "snow1" out of bound in ', self%parameter_config%file)

      case DEFAULT
        call error_message('***ERROR: Process description for process "snow" does not exist!')
    end select

    ! Process 3 - soilmoisture
    select case (self%process_matrix(3, 1))

        ! 1 - Feddes equation for PET reduction, bucket approach, Brooks-Corey like
      case(1)
        if (self%parameter_config%soilmoisture1%nml_status /= 0_i4) call error_message("'soilmoisture1' namelist not found.")

        self%process_matrix(3, 2) = 17_i4
        self%process_matrix(3, 3) = sum(self%process_matrix(1 : 3, 2))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture1%orgMatterContent_forest, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture1%orgMatterContent_impervious, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture1%orgMatterContent_pervious, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture1%PTF_lower66_5_constant, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture1%PTF_lower66_5_clay, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture1%PTF_lower66_5_Db, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture1%PTF_higher66_5_constant, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture1%PTF_higher66_5_clay, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture1%PTF_higher66_5_Db, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture1%PTF_Ks_constant, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture1%PTF_Ks_sand, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture1%PTF_Ks_clay, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture1%PTF_Ks_curveSlope, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture1%rootFractionCoefficient_forest, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture1%rootFractionCoefficient_impervious, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture1%rootFractionCoefficient_pervious, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture1%infiltrationShapeFactor, shp))

        call append(self%parameters_name, [     &
                'orgMatterContent_forest           ', &
                'orgMatterContent_impervious       ', &
                'orgMatterContent_pervious         ', &
                'PTF_lower66_5_constant            ', &
                'PTF_lower66_5_clay                ', &
                'PTF_lower66_5_Db                  ', &
                'PTF_higher66_5_constant           ', &
                'PTF_higher66_5_clay               ', &
                'PTF_higher66_5_Db                 ', &
                'PTF_Ks_constant                   ', &
                'PTF_Ks_sand                       ', &
                'PTF_Ks_clay                       ', &
                'PTF_Ks_curveSlope                 ', &
                'rootFractionCoefficient_forest    ', &
                'rootFractionCoefficient_impervious', &
                'rootFractionCoefficient_pervious  ', &
                'infiltrationShapeFactor           '])

        ! check if parameter are in range
        if (.not. in_bound(self%parameters_definition)) &
          call error_message('***ERROR: parameter in namelist "soilmoisture1" out of bound in ', self%parameter_config%file)

        ! 2- Jarvis equation for PET reduction, bucket approach, Brooks-Corey like
      case(2)
        if (self%parameter_config%soilmoisture2%nml_status /= 0_i4) call error_message("'soilmoisture2' namelist not found.")

        self%process_matrix(3, 2) = 18_i4
        self%process_matrix(3, 3) = sum(self%process_matrix(1 : 3, 2))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture2%orgMatterContent_forest, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture2%orgMatterContent_impervious, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture2%orgMatterContent_pervious, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture2%PTF_lower66_5_constant, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture2%PTF_lower66_5_clay, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture2%PTF_lower66_5_Db, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture2%PTF_higher66_5_constant, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture2%PTF_higher66_5_clay, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture2%PTF_higher66_5_Db, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture2%PTF_Ks_constant, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture2%PTF_Ks_sand, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture2%PTF_Ks_clay, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture2%PTF_Ks_curveSlope, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture2%rootFractionCoefficient_forest, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture2%rootFractionCoefficient_impervious, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture2%rootFractionCoefficient_pervious, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture2%infiltrationShapeFactor, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture2%jarvis_sm_threshold_c1, shp))

        call append(self%parameters_name, [     &
                'orgMatterContent_forest           ', &
                'orgMatterContent_impervious       ', &
                'orgMatterContent_pervious         ', &
                'PTF_lower66_5_constant            ', &
                'PTF_lower66_5_clay                ', &
                'PTF_lower66_5_Db                  ', &
                'PTF_higher66_5_constant           ', &
                'PTF_higher66_5_clay               ', &
                'PTF_higher66_5_Db                 ', &
                'PTF_Ks_constant                   ', &
                'PTF_Ks_sand                       ', &
                'PTF_Ks_clay                       ', &
                'PTF_Ks_curveSlope                 ', &
                'rootFractionCoefficient_forest    ', &
                'rootFractionCoefficient_impervious', &
                'rootFractionCoefficient_pervious  ', &
                'infiltrationShapeFactor           ', &
                'jarvis_sm_threshold_c1            '])

        ! check if parameter are in range
        if (.not. in_bound(self%parameters_definition)) &
          call error_message('***ERROR: parameter in namelist "soilmoisture2" out of bound in ', self%parameter_config%file)

        ! 3- Jarvis equation for ET reduction and FC dependency on root fraction coefficient
      case(3)
        if (self%parameter_config%soilmoisture3%nml_status /= 0_i4) call error_message("'soilmoisture3' namelist not found.")

        self%process_matrix(3, 2) = 22_i4
        self%process_matrix(3, 3) = sum(self%process_matrix(1 : 3, 2))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture3%orgMatterContent_forest, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture3%orgMatterContent_impervious, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture3%orgMatterContent_pervious, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture3%PTF_lower66_5_constant, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture3%PTF_lower66_5_clay, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture3%PTF_lower66_5_Db, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture3%PTF_higher66_5_constant, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture3%PTF_higher66_5_clay, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture3%PTF_higher66_5_Db, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture3%PTF_Ks_constant, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture3%PTF_Ks_sand, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture3%PTF_Ks_clay, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture3%PTF_Ks_curveSlope, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture3%rootFractionCoefficient_forest, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture3%rootFractionCoefficient_impervious, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture3%rootFractionCoefficient_pervious, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture3%infiltrationShapeFactor, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture3%rootFractionCoefficient_sand, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture3%rootFractionCoefficient_clay, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture3%FCmin_glob, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture3%FCdelta_glob, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture3%jarvis_sm_threshold_c1, shp))

        call append(self%parameters_name, [     &
                'orgMatterContent_forest           ', &
                'orgMatterContent_impervious       ', &
                'orgMatterContent_pervious         ', &
                'PTF_lower66_5_constant            ', &
                'PTF_lower66_5_clay                ', &
                'PTF_lower66_5_Db                  ', &
                'PTF_higher66_5_constant           ', &
                'PTF_higher66_5_clay               ', &
                'PTF_higher66_5_Db                 ', &
                'PTF_Ks_constant                   ', &
                'PTF_Ks_sand                       ', &
                'PTF_Ks_clay                       ', &
                'PTF_Ks_curveSlope                 ', &
                'rootFractionCoefficient_forest    ', &
                'rootFractionCoefficient_impervious', &
                'rootFractionCoefficient_pervious  ', &
                'infiltrationShapeFactor           ', &
                'rootFractionCoefficient_sand      ', &
                'rootFractionCoefficient_clay      ', &
                'FCmin_glob                        ', &
                'FCdelta_glob                      ', &
                'jarvis_sm_threshold_c1            '])

        ! check if parameter are in range
        if (.not. in_bound(self%parameters_definition)) &
          call error_message('***ERROR: parameter in namelist "soilmoisture3" out of bound in ', self%parameter_config%file)

        ! 4- Feddes equation for ET reduction and FC dependency on root fraction coefficient
      case(4)
        if (self%parameter_config%soilmoisture4%nml_status /= 0_i4) call error_message("'soilmoisture4' namelist not found.")

        self%process_matrix(3, 2) = 21_i4
        self%process_matrix(3, 3) = sum(self%process_matrix(1 : 3, 2))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture4%orgMatterContent_forest, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture4%orgMatterContent_impervious, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture4%orgMatterContent_pervious, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture4%PTF_lower66_5_constant, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture4%PTF_lower66_5_clay, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture4%PTF_lower66_5_Db, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture4%PTF_higher66_5_constant, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture4%PTF_higher66_5_clay, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture4%PTF_higher66_5_Db, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture4%PTF_Ks_constant, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture4%PTF_Ks_sand, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture4%PTF_Ks_clay, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture4%PTF_Ks_curveSlope, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture4%rootFractionCoefficient_forest, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture4%rootFractionCoefficient_impervious, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture4%rootFractionCoefficient_pervious, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture4%infiltrationShapeFactor, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture4%rootFractionCoefficient_sand, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture4%rootFractionCoefficient_clay, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture4%FCmin_glob, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%soilmoisture4%FCdelta_glob, shp))

        call append(self%parameters_name, [     &
                'orgMatterContent_forest           ', &
                'orgMatterContent_impervious       ', &
                'orgMatterContent_pervious         ', &
                'PTF_lower66_5_constant            ', &
                'PTF_lower66_5_clay                ', &
                'PTF_lower66_5_Db                  ', &
                'PTF_higher66_5_constant           ', &
                'PTF_higher66_5_clay               ', &
                'PTF_higher66_5_Db                 ', &
                'PTF_Ks_constant                   ', &
                'PTF_Ks_sand                       ', &
                'PTF_Ks_clay                       ', &
                'PTF_Ks_curveSlope                 ', &
                'rootFractionCoefficient_forest    ', &
                'rootFractionCoefficient_impervious', &
                'rootFractionCoefficient_pervious  ', &
                'infiltrationShapeFactor           ', &
                'rootFractionCoefficient_sand      ', &
                'rootFractionCoefficient_clay      ', &
                'FCmin_glob                        ', &
                'FCdelta_glob                      '])

        ! check if parameter are in range
        if (.not. in_bound(self%parameters_definition)) &
          call error_message('***ERROR: parameter in namelist "soilmoisture4" out of bound in ', self%parameter_config%file)

      case DEFAULT
        call error_message('***ERROR: Process description for process "soilmoisture" does not exist!')
    end select

    ! Process 4 - sealed area directRunoff
    select case (self%process_matrix(4, 1))
      ! 1 - bucket exceedance approach
      case(1)
        if (self%parameter_config%directrunoff1%nml_status /= 0_i4) call error_message("'directrunoff1' namelist not found.")

        self%process_matrix(4, 2) = 1_i4
        self%process_matrix(4, 3) = sum(self%process_matrix(1 : 4, 2))
        call append(self%parameters_definition, reshape(self%parameter_config%directrunoff1%imperviousStorageCapacity, shp))

        call append(self%parameters_name, ['imperviousStorageCapacity'])

        ! check if parameter are in range
        if (.not. in_bound(self%parameters_definition)) &
          call error_message('***ERROR: parameter in namelist "directRunoff1" out of bound in ', self%parameter_config%file)

      case DEFAULT
        call error_message('***ERROR: Process description for process "directRunoff" does not exist!')
    end select

    ! Process 5 - potential evapotranspiration (PET)
    select case (self%process_matrix(5, 1))
      case(-1) ! 0 - PET is input, correct PET by LAI
        if (self%parameter_config%petminus1%nml_status /= 0_i4) call error_message("'petminus1' namelist not found.")

        self%process_matrix(5, 2) = 5_i4
        self%process_matrix(5, 3) = sum(self%process_matrix(1 : 5, 2))
        call append(self%parameters_definition, reshape(self%parameter_config%petminus1%PET_a_forest, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%petminus1%PET_a_impervious, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%petminus1%PET_a_pervious, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%petminus1%PET_b, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%petminus1%PET_c, shp))

        call append(self%parameters_name, [ &
                'PET_a_forest     ', &
                'PET_a_impervious ', &
                'PET_a_pervious   ', &
                'PET_b            ', &
                'PET_c            '])

        ! check if parameter are in range
        if (.not. in_bound(self%parameters_definition)) &
          call error_message('***ERROR: parameter in namelist "PETminus1" out of bound  n ', self%parameter_config%file)

      case(0) ! 0 - PET is input, correct PET by aspect
        if (self%parameter_config%pet0%nml_status /= 0_i4) call error_message("'pet0' namelist not found.")

        self%process_matrix(5, 2) = 3_i4
        self%process_matrix(5, 3) = sum(self%process_matrix(1 : 5, 2))
        call append(self%parameters_definition, reshape(self%parameter_config%pet0%minCorrectionFactorPET, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%pet0%maxCorrectionFactorPET, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%pet0%aspectTresholdPET, shp))

        call append(self%parameters_name, [ &
                'minCorrectionFactorPET ', &
                'maxCorrectionFactorPET ', &
                'aspectTresholdPET      '])

        ! check if parameter are in range
        if (.not. in_bound(self%parameters_definition)) &
          call error_message('***ERROR: parameter in namelist "PET0" out of bound in ', self%parameter_config%file)

      case(1) ! 1 - Hargreaves-Samani method (HarSam) - additional input needed: Tmin, Tmax
        if (self%parameter_config%pet1%nml_status /= 0_i4) call error_message("'pet1' namelist not found.")

        self%process_matrix(5, 2) = 4_i4
        self%process_matrix(5, 3) = sum(self%process_matrix(1 : 5, 2))
        call append(self%parameters_definition, reshape(self%parameter_config%pet1%minCorrectionFactorPET, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%pet1%maxCorrectionFactorPET, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%pet1%aspectTresholdPET, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%pet1%HargreavesSamaniCoeff, shp))
        call append(self%parameters_name, [ &
                'minCorrectionFactorPET', &
                'maxCorrectionFactorPET', &
                'aspectTresholdPET     ', &
                'HargreavesSamaniCoeff '])

        ! check if parameter are in range
        if (.not. in_bound(self%parameters_definition)) &
          call error_message('***ERROR: parameter in namelist "PET1" out of bound in ', self%parameter_config%file)

      case(2) ! 2 - Priestley-Taylor method (PrieTay) - additional input needed: net_rad
        if (self%parameter_config%pet2%nml_status /= 0_i4) call error_message("'pet2' namelist not found.")

        self%process_matrix(5, 2) = 2_i4
        self%process_matrix(5, 3) = sum(self%process_matrix(1 : 5, 2))
        call append(self%parameters_definition, reshape(self%parameter_config%pet2%PriestleyTaylorCoeff, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%pet2%PriestleyTaylorLAIcorr, shp))
        call append(self%parameters_name, [ &
                'PriestleyTaylorCoeff  ', &
                'PriestleyTaylorLAIcorr'])

        ! check if parameter are in range
        if (.not. in_bound(self%parameters_definition)) &
          call error_message('***ERROR: parameter in namelist "PET2" out of bound in ', self%parameter_config%file)

      case(3) ! 3 - Penman-Monteith method - additional input needed: net_rad, abs. vapour pressue, windspeed
        if (self%parameter_config%pet3%nml_status /= 0_i4) call error_message("'pet3' namelist not found.")

        self%process_matrix(5, 2) = 7_i4
        self%process_matrix(5, 3) = sum(self%process_matrix(1 : 5, 2))

        call append(self%parameters_definition, reshape(self%parameter_config%pet3%canopyheigth_forest, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%pet3%canopyheigth_impervious, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%pet3%canopyheigth_pervious, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%pet3%displacementheight_coeff, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%pet3%roughnesslength_momentum_coeff, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%pet3%roughnesslength_heat_coeff, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%pet3%stomatal_resistance, shp))

        call append(self%parameters_name, [ &
                'canopyheigth_forest           ', &
                'canopyheigth_impervious       ', &
                'canopyheigth_pervious         ', &
                'displacementheight_coeff      ', &
                'roughnesslength_momentum_coeff', &
                'roughnesslength_heat_coeff    ', &
                'stomatal_resistance           '])

        ! check if parameter are in range
        if (.not. in_bound(self%parameters_definition)) &
          call error_message('***ERROR: parameter in namelist "PET3" out of bound in ', self%parameter_config%file)

      case DEFAULT
        call error_message('***ERROR: Process description for process "actualET" does not exist!')
    end select

    ! Process 6 - interflow
    select case (self%process_matrix(6, 1))
      ! 1 - parallel soil reservoir approach
      case(1)
        if (self%parameter_config%interflow1%nml_status /= 0_i4) call error_message("'interflow1' namelist not found.")

        self%process_matrix(6, 2) = 5_i4
        self%process_matrix(6, 3) = sum(self%process_matrix(1 : 6, 2))
        call append(self%parameters_definition, reshape(self%parameter_config%interflow1%interflowStorageCapacityFactor, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%interflow1%interflowRecession_slope, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%interflow1%fastInterflowRecession_forest, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%interflow1%slowInterflowRecession_Ks, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%interflow1%exponentSlowInterflow, shp))

        call append(self%parameters_name, [ &
                'interflowStorageCapacityFactor', &
                'interflowRecession_slope      ', &
                'fastInterflowRecession_forest ', &
                'slowInterflowRecession_Ks     ', &
                'exponentSlowInterflow         '])

        ! check if parameter are in range
        if (.not. in_bound(self%parameters_definition)) &
          call error_message('***ERROR: parameter in namelist "interflow1" out of bound in ', self%parameter_config%file)

      case DEFAULT
        call error_message('***ERROR: Process description for process "interflow" does not exist!')
    end select

    ! Process 7 - percolation
    select case (self%process_matrix(7, 1))
      ! 1 - GW layer is assumed as bucket
      case(1)
        if (self%parameter_config%percolation1%nml_status /= 0_i4) call error_message("'percolation1' namelist not found.")

        self%process_matrix(7, 2) = 3_i4
        self%process_matrix(7, 3) = sum(self%process_matrix(1 : 7, 2))
        call append(self%parameters_definition, reshape(self%parameter_config%percolation1%rechargeCoefficient, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%percolation1%rechargeFactor_karstic, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%percolation1%gain_loss_GWreservoir_karstic, shp))

        call append(self%parameters_name, [ &
                'rechargeCoefficient          ', &
                'rechargeFactor_karstic       ', &
                'gain_loss_GWreservoir_karstic'])

        ! check if parameter are in range
        if (.not. in_bound(self%parameters_definition)) &
          call error_message('***ERROR: parameter in namelist "percolation1" out of bound in ', self%parameter_config%file)

      case DEFAULT
        call error_message('***ERROR: Process description for process "percolation" does not exist!')
    end select

    ! Process 8 - routing
    select case (self%process_matrix(8, 1))
      case(0)
        ! 0 - deactivated
        call message()
        call message('***CAUTION: Routing is deativated! ')

        self%process_matrix(8, 2) = 0_i4
        self%process_matrix(8, 3) = sum(self%process_matrix(1 : 8, 2))
      case(1)
        ! parameter values and names are set in mRM
        ! 1 - Muskingum approach
        self%process_matrix(8, 2) = 5_i4
        self%process_matrix(8, 3) = sum(self%process_matrix(1 : 8, 2))
        call append(self%parameters_definition, dummy_2d_dp)
        call append(self%parameters_name, ['dummy', 'dummy', 'dummy', 'dummy', 'dummy'])
      case(2)
        self%process_matrix(8, 2) = 1_i4
        self%process_matrix(8, 3) = sum(self%process_matrix(1 : 8, 2))
        call append(self%parameters_definition, dummy_2d_dp_2)
        call append(self%parameters_name, ['dummy'])
      case(3)
        self%process_matrix(8, 2) = 1_i4
        self%process_matrix(8, 3) = sum(self%process_matrix(1 : 8, 2))
        call append(self%parameters_definition, dummy_2d_dp_2)
        call append(self%parameters_name, ['dummy'])
      case DEFAULT
        call error_message('***ERROR: Process description for process "routing" does not exist!')
    end select

    !===============================================================
    ! Geological formations
    !===============================================================
    dummy = dummy // ''   ! only to avoid warning

    ! Process 9 - geoparameter
    select case (self%process_matrix(9, 1))
      case(1)
        ! read in global parameters (NOT REGIONALIZED, i.e. these are <beta> and not <gamma>) for each geological formation used
        if (self%parameter_config%geoparameter%nml_status /= 0_i4) call error_message("'geoparameter' namelist not found.")
        GeoParam = self%parameter_config%geoparameter%GeoParam

        ! search number of geological parameters
        do ii = 1, size(GeoParam, 1) ! no while loop to avoid risk of endless loop
          if (EQ(GeoParam(ii, 1), nodata_dp)) then
            self%nGeoUnits = ii - 1
            exit
          end if
        end do

        ! for geology parameters
        self%process_matrix(9, 2) = self%nGeoUnits
        self%process_matrix(9, 3) = sum(self%process_matrix(1 : 9, 2))

        call append(self%parameters_definition, GeoParam(1 : self%nGeoUnits, :))

        ! create names
        do ii = 1, self%nGeoUnits
          dummy = 'GeoParam(' // trim(adjustl(num2str(ii))) // ',:)'
          call append(self%parameters_name, [ trim(dummy) ])
        end do

        ! check if parameter are in range
        if (.not. in_bound(self%parameters_definition)) &
          call error_message('***ERROR: parameter in namelist "geoparameter" out of bound in ', self%parameter_config%file)

      case DEFAULT
        call error_message('***ERROR: Process description for process "geoparameter" does not exist!')
    end select

    !===============================================================
    ! NEUTRON COUNT
    !===============================================================
    ! Process 10 - neutrons
    !   0 - deactivated
    !   1 - inverse N0 based on Desilets et al. 2010
    !   2 - COSMIC forward operator by Shuttlworth et al. 2013
    select case (self%process_matrix(10, 1))
      case(0)
        ! 0 - deactivated
        call message()
        call message('***SELECTION: Neutron count routine is deativated! ')

      case(1)
        ! 1 - inverse N0 based on Desilets et al. 2010
        if (self%parameter_config%neutrons1%nml_status /= 0_i4) call error_message("'neutrons1' namelist not found.")

        self%process_matrix(10,2) = 3_i4
        self%process_matrix(10,3) = sum(self%process_matrix(1:10, 2))
        call append(self%parameters_definition, reshape(self%parameter_config%neutrons1%Desilets_N0,  shp))
        call append(self%parameters_definition, reshape(self%parameter_config%neutrons1%Desilets_LW0, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%neutrons1%Desilets_LW1, shp))

        call append(self%parameters_name, [  &
                'Desilets_N0   ', &
                'Desilets_LW0  ', &
                'Desilets_LW1  '])

        ! check if parameter are in range
        if (.not. in_bound(self%parameters_definition)) &
          call error_message('***ERROR: parameter in namelist "neutrons1" out of bound in ', self%parameter_config%file)

      case(2)
        ! 2 - COSMIC version
        if (self%parameter_config%neutrons2%nml_status /= 0_i4) call error_message("'neutrons2' namelist not found.")

        self%process_matrix(10,2) = 9_i4
        self%process_matrix(10,3) = sum(self%process_matrix(1:10, 2))
        call append(self%parameters_definition, reshape(self%parameter_config%neutrons2%COSMIC_N0,     shp))
        call append(self%parameters_definition, reshape(self%parameter_config%neutrons2%COSMIC_N1,     shp))
        call append(self%parameters_definition, reshape(self%parameter_config%neutrons2%COSMIC_N2,     shp))
        call append(self%parameters_definition, reshape(self%parameter_config%neutrons2%COSMIC_alpha0, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%neutrons2%COSMIC_alpha1, shp))
        call append(self%parameters_definition, reshape(self%parameter_config%neutrons2%COSMIC_L30,    shp))
        call append(self%parameters_definition, reshape(self%parameter_config%neutrons2%COSMIC_L31,    shp))
        call append(self%parameters_definition, reshape(self%parameter_config%neutrons2%COSMIC_LW0,    shp))
        call append(self%parameters_definition, reshape(self%parameter_config%neutrons2%COSMIC_LW1,    shp))

        call append(self%parameters_name, [  &
                'COSMIC_N0     ', &
                'COSMIC_N1     ', &
                'COSMIC_N2     ', &
                'COSMIC_alpha0 ', &
                'COSMIC_alpha1 ', &
                'COSMIC_L30    ', &
                'COSMIC_L31    ', &
                'COSMIC_LW0    ', &
                'COSMIC_LW1    '])
        ! check if parameter are in range
        if (.not. in_bound(self%parameters_definition)) &
          call error_message('***ERROR: parameter in namelist "neutrons2" out of bound in ', self%parameter_config%file)

      case DEFAULT
        call error_message('***ERROR: Process description for process "NEUTRON count" does not exist!')
    end select

    self%parameters = self%parameters_definition(:, 3)

  end subroutine exchange_config_parameter

end module mo_exchange_type
