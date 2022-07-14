!> \file    mo_namelists.f90
!> \copydoc mo_namelists

!> \brief   Module containing all namelists representations.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Jul 2022
module mo_namelists

  use mo_kind, only : i4, i8, dp
  use mo_nml, only : open_nml, close_nml, position_nml
  use mo_constants, only : YearMonths
  use mo_mhm_constants, only : nOutFlxState
  use mo_common_constants, only : maxNLcovers, maxNoDomains, nColPars
  use mo_common_variables, only : nProcesses, period
  use mo_common_mHM_mRM_variables, only : nerror_model
  use mo_mpr_constants, only : maxGeoUnit, maxNoSoilHorizons

  implicit none

  !######## mo_common_read_config

  ! namelist /project_description/ &
  !   project_details, &
  !   setup_description, &
  !   simulation_type, &
  !   Conventions, &
  !   contact, &
  !   mHM_details, &
  !   history
  !
  !> \class   nml_project_description_t
  !> \brief   'project_description' namelist content
  type, public :: nml_project_description_t
    character(19) :: name = "project_description" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    character(1024) :: project_details !< project including funding instituion., PI, etc.
    character(1024) :: setup_description !< any specific description of simulation
    character(1024) :: simulation_type !< e.g. seasonal forecast, climate projection, ...
    character(256) :: Conventions !< convention used for dataset
    character(1024) :: contact !< contact details, incl. PI name
    character(1024) :: mHM_details !< developing institution, specific mHM revision
    character(1024) :: history !< details on version/creation date
  contains
    !> \copydoc mo_namelists::read_project_description
    procedure, public :: read => read_project_description !< \see mo_namelists::read_project_description
  end type nml_project_description_t
  !> 'project_description' namelist content
  type(nml_project_description_t), public :: nml_project_description

  ! namelist /directories_general/ &
  !   dirConfigOut, &
  !   dirCommonFiles, &
  !   dir_Morpho, &
  !   dir_LCover, &
  !   dir_Out, &
  !   mhm_file_RestartOut, &
  !   mrm_file_RestartOut, &
  !   file_LatLon
  !
  !> \class   nml_directories_general_t
  !> \brief   'directories_general' namelist content
  type, public :: nml_directories_general_t
    character(19) :: name = "directories_general" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    character(256) :: dirConfigOut !< directory for config file output
    character(256) :: dirCommonFiles !< directory where common input files should be located
    character(256), dimension(maxNoDomains) :: mhm_file_RestartOut !< Directory where mhm output of restart is written
    character(256), dimension(maxNoDomains) :: mrm_file_RestartOut !< Directory where mrm output of restart is written
    character(256), dimension(maxNoDomains) :: dir_Morpho !< Directory where morphological files are located
    character(256), dimension(maxNoDomains) :: dir_LCover !< Directory where land cover files are located
    character(256), dimension(maxNoDomains) :: dir_Out !< Directory where output is written to
    character(256), dimension(maxNoDomains) :: file_LatLon !< Directory where the Lat Lon Files are located
  contains
    !> \copydoc mo_namelists::read_directories_general
    procedure, public :: read => read_directories_general !< \see mo_namelists::read_directories_general
  end type nml_directories_general_t
  !> 'directories_general' namelist content
  type(nml_directories_general_t), public :: nml_directories_general

  ! namelist /mainconfig/ &
  !   iFlag_cordinate_sys, &
  !   resolution_Hydrology, &
  !   nDomains, &
  !   L0Domain, &
  !   write_restart, &
  !   read_opt_domain_data
  !
  !> \class   nml_mainconfig_t
  !> \brief   'mainconfig' namelist content
  type, public :: nml_mainconfig_t
    character(10) :: name = "mainconfig" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    integer(i4) :: iFlag_cordinate_sys !< options model for the run cordinate system
    real(dp), dimension(maxNoDomains) :: resolution_Hydrology !< [m or degree] resolution of hydrology - Level 1
    integer(i4) :: nDomains !< number of domains
    integer(i4), dimension(maxNoDomains) :: L0Domain !< specify same index for domains to share L0_data to save memory
    logical :: write_restart !< flag to write restart
    integer(i4), dimension(maxNoDomains) :: read_opt_domain_data !< read domain specific optional data
    contains
    !> \copydoc mo_namelists::read_mainconfig
    procedure, public :: read => read_mainconfig !< \see mo_namelists::read_mainconfig
  end type nml_mainconfig_t
  !> 'mainconfig' namelist content
  type(nml_mainconfig_t), public :: nml_mainconfig

  ! namelist /processSelection/ &
  !   processCase
  !
  !> \class   nml_processselection_t
  !> \brief   'processSelection' namelist content
  type, public :: nml_processselection_t
    character(16) :: name = "processSelection" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    integer(i4), dimension(nProcesses) :: processCase !< ! Choosen process description number
  contains
    !> \copydoc mo_namelists::read_processselection
    procedure, public :: read => read_processselection !< \see mo_namelists::read_processselection
  end type nml_processselection_t
  !> 'processSelection' namelist content
  type(nml_processselection_t), public :: nml_processselection

  ! namelist /LCover/ &
  !   nLcoverScene, &
  !   LCoverYearStart, &
  !   LCoverYearEnd, &
  !   LCoverfName
  !
  !> \class   nml_lcover_t
  !> \brief   'LCover' namelist content
  type, public :: nml_lcover_t
    character(6) :: name = "LCover" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    integer(i4) :: nLCoverScene !< Number of land cover scene (lcs)
    integer(i4), dimension(maxNLCovers) :: LCoverYearStart !< starting year LCover
    integer(i4), dimension(maxNLCovers) :: LCoverYearEnd !< ending year LCover
    character(256), dimension(maxNLCovers) :: LCoverfName !< filename of Lcover file
  contains
    !> \copydoc mo_namelists::read_lcover
    procedure, public :: read => read_lcover !< \see mo_namelists::read_lcover
  end type nml_lcover_t
  !> 'LCover' namelist content
  type(nml_lcover_t), public :: nml_lcover

  !######## mo_mHM_mRM_read_config

  ! namelist /mainconfig_mhm_mrm/ &
  !   timestep, &
  !   resolution_Routing, &
  !   optimize, &
  !   optimize_restart, &
  !   opti_method, &
  !   opti_function, &
  !   read_restart, &
  !   mrm_read_river_network, &
  !   read_old_style_restart_bounds, &
  !   mhm_file_RestartIn, &
  !   mrm_file_RestartIn
  !
  !> \class   nml_mainconfig_mhm_mrm_t
  !> \brief   'mainconfig_mhm_mrm' namelist content
  type, public :: nml_mainconfig_mhm_mrm_t
    character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    integer(i4) :: timeStep !< [h] simulation time step (= TS) in [h] either 1, 2, 3, 4, 6, 12 or 24
    real(dp), dimension(maxNoDomains) :: resolution_Routing !< resolution of Level-11 discharge routing [m or degree] per domain
    logical :: optimize !< Optimization (.true.) or Evaluation run (.false.)
    logical :: optimize_restart !< Optimization will be restarted from mo_<opti_method>.restart file (.true.)
    integer(i4) :: opti_method !< Optimization algorithm: 1 - DDS; 2 - Simulated Annealing; 3 - SCE
    integer(i4) :: opti_function !< Objective function
    logical :: read_restart !< flag for reading restart output
    logical :: mrm_read_river_network !< flag to read the river network for mRM (read_restart = .True. forces .True.)
    logical :: read_old_style_restart_bounds !< flag to use an old-style restart file created by mhm<=v5.11
    character(256), dimension(maxNoDomains) :: mhm_file_RestartIn !< mhm restart file paths
    character(256), dimension(maxNoDomains) :: mrm_file_RestartIn !< mrm restart file paths
  contains
    !> \copydoc mo_namelists::read_mainconfig_mhm_mrm
    procedure, public :: read => read_mainconfig_mhm_mrm !< \see mo_namelists::read_mainconfig_mhm_mrm
  end type nml_mainconfig_mhm_mrm_t
  !> 'mainconfig_mhm_mrm' namelist content
  type(nml_mainconfig_mhm_mrm_t), public :: nml_mainconfig_mhm_mrm

  ! namelist /Optimization/ &
  !   nIterations, &
  !   seed, &
  !   dds_r, &
  !   sa_temp, &
  !   sce_ngs, &
  !   sce_npg, &
  !   sce_nps, &
  !   mcmc_opti, &
  !   mcmc_error_params
  !
  !> \class   nml_optimization_t
  !> \brief   'Optimization' namelist content
  type, public :: nml_Optimization_t
    character(12) :: name = "Optimization" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    integer(i4) :: nIterations !< number of iterations for optimization
    integer(i8) :: seed !< seed used for optimization, default: -9 --> system time
    real(dp) :: dds_r !< DDS: perturbation rate, default: 0.2
    real(dp) :: sa_temp !< SA:  initial temperature, default: -9.0 --> estimated
    integer(i4) :: sce_ngs !< SCE: # of complexes, default: 2
    integer(i4) :: sce_npg !< SCE: # of points per complex,default: -9 --> 2n+1
    integer(i4) :: sce_nps !< SCE: # of points per subcomplex,default: -9 --> n+1
    logical :: mcmc_opti !< MCMC: Optimization (.true.) or only parameter uncertainty (.false.)
    real(dp), dimension(nerror_model) :: mcmc_error_params !< error model para (mcmc_opti=.false.) e.g. for opti_function=8: .01, .3
  contains
    !> \copydoc mo_namelists::read_Optimization
    procedure, public :: read => read_Optimization !< \see mo_namelists::read_Optimization
  end type nml_Optimization_t
  !> 'Optimization' namelist content
  type(nml_Optimization_t), public :: nml_Optimization

  ! namelist /time_periods/ &
  !   warming_Days, &
  !   eval_Per
  !
  !> \class   nml_time_periods_t
  !> \brief   'time_periods' namelist content
  type, public :: nml_time_periods_t
    character(12) :: name = "time_periods" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    integer(i4), dimension(maxNoDomains) :: warming_Days !< number of days for warm up period
    type(period), dimension(maxNoDomains) :: eval_Per !< time period for model evaluation
  contains
    !> \copydoc mo_namelists::read_time_periods
    procedure, public :: read => read_time_periods !< \see mo_namelists::read_time_periods
  end type nml_time_periods_t
  !> 'time_periods' namelist content
  type(nml_time_periods_t), public :: nml_time_periods

  !######## mo_mhm_read_config

  ! namelist /directories_mHM/ &
  !   inputFormat_meteo_forcings, &
  !   dir_Precipitation, &
  !   dir_Temperature, &
  !   dir_ReferenceET, &
  !   dir_MinTemperature, &
  !   dir_MaxTemperature, &
  !   dir_absVapPressure, &
  !   dir_windspeed, &
  !   dir_NetRadiation, &
  !   dir_Radiation, &
  !   time_step_model_inputs
  !
  !> \class   nml_directories_mhm_t
  !> \brief   'directories_mHM' namelist content
  type, public :: nml_directories_mHM_t
    character(15) :: name = "directories_mHM" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    character(256), public :: inputFormat_meteo_forcings !< format of meteo input data (nc)
    character(256), dimension(maxNoDomains) :: dir_Precipitation !< Directory where precipitation files are located
    character(256), dimension(maxNoDomains) :: dir_Temperature !< Directory where temperature files are located
    character(256), dimension(maxNoDomains) :: dir_ReferenceET !< Directory where reference-ET files are located
    character(256), dimension(maxNoDomains) :: dir_MinTemperature !< Directory where minimum temp. files are located
    character(256), dimension(maxNoDomains) :: dir_MaxTemperature !< Directory where maximum temp. files are located
    character(256), dimension(maxNoDomains) :: dir_absVapPressure !< Directory where abs. vap. pressure files are located
    character(256), dimension(maxNoDomains) :: dir_windspeed !< Directory where windspeed files are located
    character(256), dimension(maxNoDomains) :: dir_NetRadiation !< Directory where abs. vap. pressure files are located
    character(256), dimension(maxNoDomains) :: dir_Radiation !< riv-temp related: directory of (long/short-wave)radiation
    integer(i4), dimension(maxNoDomains) :: time_step_model_inputs !< frequency for reading meteo input
  contains
    !> \copydoc mo_namelists::read_directories_mHM
    procedure, public :: read => read_directories_mHM !< \see mo_namelists::read_directories_mHM
  end type nml_directories_mHM_t
  !> 'directories_mHM' namelist content
  type(nml_directories_mHM_t), public :: nml_directories_mHM

  ! namelist /optional_data/ &
  !   nSoilHorizons_sm_input, &
  !   dir_soil_moisture, &
  !   dir_neutrons, &
  !   dir_evapotranspiration, &
  !   dir_TWS, &
  !   timeStep_sm_input, &
  !   timeStep_neutrons_input, &
  !   timeStep_et_input, &
  !   timeStep_tws_input
  !
  !> \class   nml_optional_data_t
  !> \brief   'optional_data' namelist content
  type, public :: nml_optional_data_t
    character(13) :: name = "optional_data" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    integer(i4) :: nSoilHorizons_sm_input !< No. of mhm soil horizons equivalent to sm input
    character(256), dimension(maxNoDomains) :: dir_soil_moisture !< soil moisture input
    character(256), dimension(maxNoDomains) :: dir_neutrons !< ground albedo neutron input
    character(256), dimension(maxNoDomains) :: dir_evapotranspiration !< evapotranspiration input
    character(256), dimension(maxNoDomains) :: dir_TWS !< tws input
    integer(i4) :: timeStep_sm_input !< time step of optional data: sm
    integer(i4) :: timeStep_neutrons_input !< time step of optional data: neutrons
    integer(i4) :: timeStep_et_input !< time step of optional data: et
    integer(i4) :: timeStep_tws_input !< time step of optional data: tws
  contains
    !> \copydoc mo_namelists::read_optional_data
    procedure, public :: read => read_optional_data !< \see mo_namelists::read_optional_data
  end type nml_optional_data_t
  !> 'optional_data' namelist content
  type(nml_optional_data_t), public :: nml_optional_data

  ! namelist /panEvapo/ &
  !   evap_coeff
  !
  !> \class   nml_panevapo_t
  !> \brief   'panEvapo' namelist content
  type, public :: nml_panEvapo_t
    character(8) :: name = "panEvapo" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    real(dp), dimension(int(YearMonths, i4)) :: evap_coeff !< [-] Evap. coef. for free-water surfaces
  contains
    !> \copydoc mo_namelists::read_panEvapo
    procedure, public :: read => read_panEvapo !< \see mo_namelists::read_panEvapo
  end type nml_panEvapo_t
  !> 'panEvapo' namelist content
  type(nml_panEvapo_t), public :: nml_panEvapo

  ! namelist /nightDayRatio/ &
  !   read_meteo_weights, &
  !   fnight_prec, &
  !   fnight_pet, &
  !   fnight_temp, &
  !   fnight_ssrd, &
  !   fnight_strd
  !
  !> \class   nml_nightdayratio_t
  !> \brief   'nightDayRatio' namelist content
  type, public :: nml_nightDayRatio_t
    character(13) :: name = "nightDayRatio" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    logical :: read_meteo_weights !< read weights for meteo data
    real(dp), dimension(int(YearMonths, i4)) :: fnight_prec !< [-] Night ratio precipitation < 1
    real(dp), dimension(int(YearMonths, i4)) :: fnight_pet !< [-] Night ratio PET  < 1
    real(dp), dimension(int(YearMonths, i4)) :: fnight_temp !< [-] Night factor mean temp
    real(dp), dimension(int(YearMonths, i4)) :: fnight_ssrd !< [-] Night factor short-wave rad.
    real(dp), dimension(int(YearMonths, i4)) :: fnight_strd !< [-] Night factor long-wave rad.
  contains
    !> \copydoc mo_namelists::read_nightDayRatio
    procedure, public :: read => read_nightDayRatio !< \see mo_namelists::read_nightDayRatio
  end type nml_nightDayRatio_t
  !> 'nightDayRatio' namelist content
  type(nml_nightDayRatio_t), public :: nml_nightDayRatio

  ! namelist /NLoutputResults/ &
  !   output_deflate_level, &
  !   output_double_precision, &
  !   timeStep_model_outputs, &
  !   outputFlxState
  !
  !> \class   nml_nloutputresults_t
  !> \brief   'NLoutputResults' namelist content
  type, public :: nml_NLoutputResults_t
    character(15) :: name = "NLoutputResults" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    integer(i4) :: output_deflate_level !< deflate level in nc files
    logical :: output_double_precision !< output precision in nc files
    integer(i4) :: timeStep_model_outputs !< timestep for writing model outputs
    logical, dimension(nOutFlxState) :: outputFlxState !< Define model outputs see "mhm_outputs.nml"
  contains
    !> \copydoc mo_namelists::read_NLoutputResults
    procedure, public :: read => read_NLoutputResults !< \see mo_namelists::read_NLoutputResults
  end type nml_NLoutputResults_t
  !> 'NLoutputResults' namelist content
  type(nml_NLoutputResults_t), public :: nml_NLoutputResults

  ! namelist /BFI_inputs/ &
  !   BFI_calc, &
  !   BFI_obs
  !
  !> \class   nml_bfi_inputs_t
  !> \brief   'BFI_inputs' namelist content
  type, public :: nml_BFI_inputs_t
    character(10) :: name = "BFI_inputs" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    logical :: BFI_calc !< calculate observed BFI from gauges with Eckhardt filter
    real(dp), dimension(maxNoDomains) :: BFI_obs !< given base-flow index per domain
contains
    !> \copydoc mo_namelists::read_BFI_inputs
    procedure, public :: read => read_BFI_inputs !< \see mo_namelists::read_BFI_inputs
  end type nml_BFI_inputs_t
  !> 'BFI_inputs' namelist content
  type(nml_BFI_inputs_t), public :: nml_BFI_inputs

  !######## mo_mpr_read_config
  ! namelist /directories_MPR/ &
  !   dir_gridded_LAI
  !
  !> \class   nml_directories_mpr_t
  !> \brief   'directories_mpr' namelist content
  type, public :: nml_directories_mpr_t
    character(15) :: name = "directories_mpr" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    character(256), dimension(maxNoDomains) :: dir_gridded_LAI !< directory of gridded LAI data, used when timeStep_LAI_input<0
  contains
    !> \copydoc mo_namelists::read_directories_mpr
    procedure, public :: read => read_directories_mpr !< \see mo_namelists::read_directories_mpr
  end type nml_directories_mpr_t
  !> 'directories_mpr' namelist content
  type(nml_directories_mpr_t), public :: nml_directories_mpr

  ! namelist /soildata/ &
  !   iFlag_soilDB, &
  !   tillageDepth, &
  !   nSoilHorizons_mHM, &
  !   soil_Depth
  !
  !> \class   nml_soildata_t
  !> \brief   'soildata' namelist content
  type, public :: nml_soildata_t
    character(8) :: name = "soildata" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    integer(i4) :: iFlag_soilDB !< options to handle different soil databases
    real(dp) :: tillageDepth !< [mm] Soil depth down to which organic
    integer(i4) :: nSoilHorizons_mHM !< Number of horizons to model
    real(dp), dimension(maxNoSoilHorizons) :: soil_Depth !< depth of the single horizons
  contains
    !> \copydoc mo_namelists::read_soildata
    procedure, public :: read => read_soildata !< \see mo_namelists::read_soildata
  end type nml_soildata_t
  !> 'soildata' namelist content
  type(nml_soildata_t), public :: nml_soildata

  ! namelist /LAI_data_information/ &
  !   inputFormat_gridded_LAI, &
  !   timeStep_LAI_input
  !
  !> \class   nml_lai_data_information_t
  !> \brief   'lai_data_information' namelist content
  type, public :: nml_lai_data_information_t
    character(20) :: name = "lai_data_information" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    character(256) :: inputFormat_gridded_LAI !< format of gridded LAI data (nc only)
    integer(i4) :: timeStep_LAI_input !< time step of gridded LAI input
  contains
    !> \copydoc mo_namelists::read_lai_data_information
    procedure, public :: read => read_lai_data_information !< \see mo_namelists::read_lai_data_information
  end type nml_lai_data_information_t
  !> 'lai_data_information' namelist content
  type(nml_lai_data_information_t), public :: nml_lai_data_information

  ! namelist /LCover_MPR/ &
  !   fracSealed_cityArea
  !
  !> \class   nml_lcover_mpr_t
  !> \brief   'lcover_mpr' namelist content
  type, public :: nml_lcover_mpr_t
    character(10) :: name = "lcover_mpr" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    real(dp) :: fracSealed_cityArea !< fraction of area within city assumed to be perfectly sealed [0-1]
  contains
    !> \copydoc mo_namelists::read_lcover_mpr
    procedure, public :: read => read_lcover_mpr !< \see mo_namelists::read_lcover_mpr
  end type nml_lcover_mpr_t
  !> 'lcover_mpr' namelist content
  type(nml_lcover_mpr_t), public :: nml_lcover_mpr

  ! namelist /interception1/ &
  !   canopyInterceptionFactor
  !
  !> \class   nml_interception1_t
  !> \brief   'interception1' namelist content
  type, public :: nml_interception1_t
    character(13) :: name = "interception1" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    real(dp), dimension(nColPars) :: canopyInterceptionFactor !< multiplier to relate LAI to interception storage [-]
  contains
    !> \copydoc mo_namelists::read_interception1
    procedure, public :: read => read_interception1 !< \see mo_namelists::read_interception1
  end type nml_interception1_t
  !> 'interception1' namelist content
  type(nml_interception1_t), public :: nml_interception1

  ! namelist /snow1/ &
  !   snowTreshholdTemperature, &
  !   degreeDayFactor_forest, &
  !   degreeDayFactor_impervious, &
  !   degreeDayFactor_pervious, &
  !   increaseDegreeDayFactorByPrecip, &
  !   maxDegreeDayFactor_forest, &
  !   maxDegreeDayFactor_impervious, &
  !   maxDegreeDayFactor_pervious
  !
  !> \class   nml_snow1_t
  !> \brief   'snow1' namelist content
  type, public :: nml_snow1_t
    character(5) :: name = "snow1" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    real(dp), dimension(nColPars) :: snowTreshholdTemperature !< Threshold for rain/snow partitioning [degC]
    real(dp), dimension(nColPars) :: degreeDayFactor_forest !< forest: deg day factors to determine melting flux [m degC-1]
    real(dp), dimension(nColPars) :: degreeDayFactor_impervious !< impervious: deg day factors to determine melting flux [m degC-1]
    real(dp), dimension(nColPars) :: degreeDayFactor_pervious !< pervious: deg day factors to determine melting flux [m degC-1]
    real(dp), dimension(nColPars) :: increaseDegreeDayFactorByPrecip !< increase of deg day factor in case of precipitation [degC-1]
    real(dp), dimension(nColPars) :: maxDegreeDayFactor_forest !< forest: maximum values for degree day factor [m degC-1]
    real(dp), dimension(nColPars) :: maxDegreeDayFactor_impervious !< impervious: maximum values for degree day factor [m degC-1]
    real(dp), dimension(nColPars) :: maxDegreeDayFactor_pervious !< pervious: maximum values for degree day factor [m degC-1]
  contains
    !> \copydoc mo_namelists::read_snow1
    procedure, public :: read => read_snow1 !< \see mo_namelists::read_snow1
  end type nml_snow1_t
  !> 'snow1' namelist content
  type(nml_snow1_t), public :: nml_snow1

  ! namelist /soilmoisture1/ &
  !   orgMatterContent_forest, &
  !   orgMatterContent_impervious, &
  !   orgMatterContent_pervious, &
  !   PTF_lower66_5_constant, &
  !   PTF_lower66_5_clay, &
  !   PTF_lower66_5_Db, &
  !   PTF_higher66_5_constant, &
  !   PTF_higher66_5_clay, &
  !   PTF_higher66_5_Db, &
  !   PTF_Ks_constant, &
  !   PTF_Ks_sand, &
  !   PTF_Ks_clay, &
  !   PTF_Ks_curveSlope, &
  !   rootFractionCoefficient_forest, &
  !   rootFractionCoefficient_impervious, &
  !   rootFractionCoefficient_pervious, &
  !   infiltrationShapeFactor
  !
  !> \class   nml_soilmoisture1_t
  !> \brief   'soilmoisture1' namelist content
  type, public :: nml_soilmoisture1_t
    character(13) :: name = "soilmoisture1" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    real(dp), dimension(nColPars) :: orgMatterContent_forest !< organic matter content [%] for forest
    real(dp), dimension(nColPars) :: orgMatterContent_impervious !< organic matter content [%] for impervious
    real(dp), dimension(nColPars) :: orgMatterContent_pervious !< organic matter content [%] for pervious
    !> Zacharias PTF parameters below 66.5 % sand content (Zacharias et al., 2007, doi:10.2136/sssaj2006.0098)
    real(dp), dimension(nColPars) :: PTF_lower66_5_constant
    real(dp), dimension(nColPars) :: PTF_lower66_5_clay !< multiplier for clay constant (see PTF_lower66_5_constant)
    real(dp), dimension(nColPars) :: PTF_lower66_5_Db !< multiplier for mineral bulk density (see PTF_lower66_5_constant)
    !> Zacharias PTF parameters above 66.5 % sand content (Zacharias et al., 2007, doi:10.2136/sssaj2006.0098)
    real(dp), dimension(nColPars) :: PTF_higher66_5_constant
    real(dp), dimension(nColPars) :: PTF_higher66_5_clay !< multiplier for clay constant (see PTF_higher66_5_constant)
    real(dp), dimension(nColPars) :: PTF_higher66_5_Db !< multiplier for mineral bulk density (see PTF_higher66_5_constant)
    !> PTF parameters for saturated hydraulic conductivity after Cosby et al. (1984)
    real(dp), dimension(nColPars) :: PTF_Ks_constant
    real(dp), dimension(nColPars) :: PTF_Ks_sand !< multiplier for sand (see PTF_Ks_constant)
    real(dp), dimension(nColPars) :: PTF_Ks_clay !< multiplier for clay (see PTF_Ks_constant)
    real(dp), dimension(nColPars) :: PTF_Ks_curveSlope !< unit conversion factor from inch/h to cm/d
    !> shape factor for root distribution with depth, which follows an exponential function [-] for forest
    real(dp), dimension(nColPars) :: rootFractionCoefficient_forest
    !> shape factor for root distribution with depth, which follows an exponential function [-] for impervious
    real(dp), dimension(nColPars) :: rootFractionCoefficient_impervious
    !> shape factor for root distribution with depth, which follows an exponential function [-] for pervious
    real(dp), dimension(nColPars) :: rootFractionCoefficient_pervious
    !> shape factor for partitioning effective precipitation into runoff and infiltration based on soil wetness [-]
    real(dp), dimension(nColPars) :: infiltrationShapeFactor
  contains
    !> \copydoc mo_namelists::read_soilmoisture1
    procedure, public :: read => read_soilmoisture1 !< \see mo_namelists::read_soilmoisture1
  end type nml_soilmoisture1_t
  !> 'soilmoisture1' namelist content
  type(nml_soilmoisture1_t), public :: nml_soilmoisture1

  ! namelist /soilmoisture2/ &
  !   orgMatterContent_forest, &
  !   orgMatterContent_impervious, &
  !   orgMatterContent_pervious, &
  !   PTF_lower66_5_constant, &
  !   PTF_lower66_5_clay, &
  !   PTF_lower66_5_Db, &
  !   PTF_higher66_5_constant, &
  !   PTF_higher66_5_clay, &
  !   PTF_higher66_5_Db, &
  !   PTF_Ks_constant, &
  !   PTF_Ks_sand, &
  !   PTF_Ks_clay, &
  !   PTF_Ks_curveSlope, &
  !   rootFractionCoefficient_forest, &
  !   rootFractionCoefficient_impervious, &
  !   rootFractionCoefficient_pervious, &
  !   infiltrationShapeFactor, &
  !   jarvis_sm_threshold_c1
  !
  !> \class   nml_soilmoisture2_t
  !> \brief   'soilmoisture2' namelist content
  type, public :: nml_soilmoisture2_t
    character(13) :: name = "soilmoisture2" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    real(dp), dimension(nColPars) :: orgMatterContent_forest !< organic matter content [%] for forest
    real(dp), dimension(nColPars) :: orgMatterContent_impervious !< organic matter content [%] for impervious
    real(dp), dimension(nColPars) :: orgMatterContent_pervious !< organic matter content [%] for pervious
    !> Zacharias PTF parameters below 66.5 % sand content (Zacharias et al., 2007, doi:10.2136/sssaj2006.0098)
    real(dp), dimension(nColPars) :: PTF_lower66_5_constant
    real(dp), dimension(nColPars) :: PTF_lower66_5_clay !< multiplier for clay constant (see PTF_lower66_5_constant)
    real(dp), dimension(nColPars) :: PTF_lower66_5_Db !< multiplier for mineral bulk density (see PTF_lower66_5_constant)
    !> Zacharias PTF parameters above 66.5 % sand content (Zacharias et al., 2007, doi:10.2136/sssaj2006.0098)
    real(dp), dimension(nColPars) :: PTF_higher66_5_constant
    real(dp), dimension(nColPars) :: PTF_higher66_5_clay !< multiplier for clay constant (see PTF_higher66_5_constant)
    real(dp), dimension(nColPars) :: PTF_higher66_5_Db !< multiplier for mineral bulk density (see PTF_higher66_5_constant)
    !> PTF parameters for saturated hydraulic conductivity after Cosby et al. (1984)
    real(dp), dimension(nColPars) :: PTF_Ks_constant
    real(dp), dimension(nColPars) :: PTF_Ks_sand !< multiplier for sand (see PTF_Ks_constant)
    real(dp), dimension(nColPars) :: PTF_Ks_clay !< multiplier for clay (see PTF_Ks_constant)
    real(dp), dimension(nColPars) :: PTF_Ks_curveSlope !< unit conversion factor from inch/h to cm/d
    !> shape factor for root distribution with depth, which follows an exponential function [-] for forest
    real(dp), dimension(nColPars) :: rootFractionCoefficient_forest
    !> shape factor for root distribution with depth, which follows an exponential function [-] for impervious
    real(dp), dimension(nColPars) :: rootFractionCoefficient_impervious
    !> shape factor for root distribution with depth, which follows an exponential function [-] for pervious
    real(dp), dimension(nColPars) :: rootFractionCoefficient_pervious
    !> shape factor for partitioning effective precipitation into runoff and infiltration based on soil wetness [-]
    real(dp), dimension(nColPars) :: infiltrationShapeFactor
    real(dp), dimension(nColPars) :: jarvis_sm_threshold_c1 !< soil moisture threshod for jarvis model
  contains
    !> \copydoc mo_namelists::read_soilmoisture2
    procedure, public :: read => read_soilmoisture2 !< \see mo_namelists::read_soilmoisture2
  end type nml_soilmoisture2_t
  !> 'soilmoisture2' namelist content
  type(nml_soilmoisture2_t), public :: nml_soilmoisture2

  ! namelist /soilmoisture3/ &
  !   orgMatterContent_forest, &
  !   orgMatterContent_impervious, &
  !   orgMatterContent_pervious, &
  !   PTF_lower66_5_constant, &
  !   PTF_lower66_5_clay, &
  !   PTF_lower66_5_Db, &
  !   PTF_higher66_5_constant, &
  !   PTF_higher66_5_clay, &
  !   PTF_higher66_5_Db, &
  !   PTF_Ks_constant, &
  !   PTF_Ks_sand, &
  !   PTF_Ks_clay, &
  !   PTF_Ks_curveSlope, &
  !   rootFractionCoefficient_forest, &
  !   rootFractionCoefficient_impervious, &
  !   rootFractionCoefficient_pervious, &
  !   infiltrationShapeFactor, &
  !   rootFractionCoefficient_sand, &
  !   rootFractionCoefficient_clay, &
  !   FCmin_glob, &
  !   FCdelta_glob, &
  !   jarvis_sm_threshold_c1
  !
  !> \class   nml_soilmoisture3_t
  !> \brief   'soilmoisture3' namelist content
  type, public :: nml_soilmoisture3_t
    character(13) :: name = "soilmoisture3" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    real(dp), dimension(nColPars) :: orgMatterContent_forest !< organic matter content [%] for forest
    real(dp), dimension(nColPars) :: orgMatterContent_impervious !< organic matter content [%] for impervious
    real(dp), dimension(nColPars) :: orgMatterContent_pervious !< organic matter content [%] for pervious
    !> Zacharias PTF parameters below 66.5 % sand content (Zacharias et al., 2007, doi:10.2136/sssaj2006.0098)
    real(dp), dimension(nColPars) :: PTF_lower66_5_constant
    real(dp), dimension(nColPars) :: PTF_lower66_5_clay !< multiplier for clay constant (see PTF_lower66_5_constant)
    real(dp), dimension(nColPars) :: PTF_lower66_5_Db !< multiplier for mineral bulk density (see PTF_lower66_5_constant)
    !> Zacharias PTF parameters above 66.5 % sand content (Zacharias et al., 2007, doi:10.2136/sssaj2006.0098)
    real(dp), dimension(nColPars) :: PTF_higher66_5_constant
    real(dp), dimension(nColPars) :: PTF_higher66_5_clay !< multiplier for clay constant (see PTF_higher66_5_constant)
    real(dp), dimension(nColPars) :: PTF_higher66_5_Db !< multiplier for mineral bulk density (see PTF_higher66_5_constant)
    !> PTF parameters for saturated hydraulic conductivity after Cosby et al. (1984)
    real(dp), dimension(nColPars) :: PTF_Ks_constant
    real(dp), dimension(nColPars) :: PTF_Ks_sand !< multiplier for sand (see PTF_Ks_constant)
    real(dp), dimension(nColPars) :: PTF_Ks_clay !< multiplier for clay (see PTF_Ks_constant)
    real(dp), dimension(nColPars) :: PTF_Ks_curveSlope !< unit conversion factor from inch/h to cm/d
    !> shape factor for root distribution with depth, which follows an exponential function [-] for forest
    real(dp), dimension(nColPars) :: rootFractionCoefficient_forest
    !> shape factor for root distribution with depth, which follows an exponential function [-] for impervious
    real(dp), dimension(nColPars) :: rootFractionCoefficient_impervious
    !> shape factor for root distribution with depth, which follows an exponential function [-] for pervious
    real(dp), dimension(nColPars) :: rootFractionCoefficient_pervious
    !> shape factor for partitioning effective precipitation into runoff and infiltration based on soil wetness [-]
    real(dp), dimension(nColPars) :: infiltrationShapeFactor
    real(dp), dimension(nColPars) :: FCmin_glob !< global field capacity minimum
    real(dp), dimension(nColPars) :: FCdelta_glob !< difference between global field capacity minimum and maximum
    real(dp), dimension(nColPars) :: rootFractionCoefficient_sand !< threshold for actual ET reduction for sand
    real(dp), dimension(nColPars) :: rootFractionCoefficient_clay !< threshold for actual ET reduction for clay
    real(dp), dimension(nColPars) :: jarvis_sm_threshold_c1 !< soil moisture threshod for jarvis model
  contains
    !> \copydoc mo_namelists::read_soilmoisture3
    procedure, public :: read => read_soilmoisture3 !< \see mo_namelists::read_soilmoisture3
  end type nml_soilmoisture3_t
  !> 'soilmoisture3' namelist content
  type(nml_soilmoisture3_t), public :: nml_soilmoisture3

  ! namelist /soilmoisture4/ &
  !   orgMatterContent_forest, &
  !   orgMatterContent_impervious, &
  !   orgMatterContent_pervious, &
  !   PTF_lower66_5_constant, &
  !   PTF_lower66_5_clay, &
  !   PTF_lower66_5_Db, &
  !   PTF_higher66_5_constant, &
  !   PTF_higher66_5_clay, &
  !   PTF_higher66_5_Db, &
  !   PTF_Ks_constant, &
  !   PTF_Ks_sand, &
  !   PTF_Ks_clay, &
  !   PTF_Ks_curveSlope, &
  !   rootFractionCoefficient_forest, &
  !   rootFractionCoefficient_impervious, &
  !   rootFractionCoefficient_pervious, &
  !   infiltrationShapeFactor, &
  !   rootFractionCoefficient_sand, &
  !   rootFractionCoefficient_clay, &
  !   FCmin_glob, &
  !   FCdelta_glob, &
  !
  !> \class   nml_soilmoisture4_t
  !> \brief   'soilmoisture4' namelist content
  type, public :: nml_soilmoisture4_t
    character(13) :: name = "soilmoisture4" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    real(dp), dimension(nColPars) :: orgMatterContent_forest !< organic matter content [%] for forest
    real(dp), dimension(nColPars) :: orgMatterContent_impervious !< organic matter content [%] for impervious
    real(dp), dimension(nColPars) :: orgMatterContent_pervious !< organic matter content [%] for pervious
    !> Zacharias PTF parameters below 66.5 % sand content (Zacharias et al., 2007, doi:10.2136/sssaj2006.0098)
    real(dp), dimension(nColPars) :: PTF_lower66_5_constant
    real(dp), dimension(nColPars) :: PTF_lower66_5_clay !< multiplier for clay constant (see PTF_lower66_5_constant)
    real(dp), dimension(nColPars) :: PTF_lower66_5_Db !< multiplier for mineral bulk density (see PTF_lower66_5_constant)
    !> Zacharias PTF parameters above 66.5 % sand content (Zacharias et al., 2007, doi:10.2136/sssaj2006.0098)
    real(dp), dimension(nColPars) :: PTF_higher66_5_constant
    real(dp), dimension(nColPars) :: PTF_higher66_5_clay !< multiplier for clay constant (see PTF_higher66_5_constant)
    real(dp), dimension(nColPars) :: PTF_higher66_5_Db !< multiplier for mineral bulk density (see PTF_higher66_5_constant)
    !> PTF parameters for saturated hydraulic conductivity after Cosby et al. (1984)
    real(dp), dimension(nColPars) :: PTF_Ks_constant
    real(dp), dimension(nColPars) :: PTF_Ks_sand !< multiplier for sand (see PTF_Ks_constant)
    real(dp), dimension(nColPars) :: PTF_Ks_clay !< multiplier for clay (see PTF_Ks_constant)
    real(dp), dimension(nColPars) :: PTF_Ks_curveSlope !< unit conversion factor from inch/h to cm/d
    !> shape factor for root distribution with depth, which follows an exponential function [-] for forest
    real(dp), dimension(nColPars) :: rootFractionCoefficient_forest
    !> shape factor for root distribution with depth, which follows an exponential function [-] for impervious
    real(dp), dimension(nColPars) :: rootFractionCoefficient_impervious
    !> shape factor for root distribution with depth, which follows an exponential function [-] for pervious
    real(dp), dimension(nColPars) :: rootFractionCoefficient_pervious
    !> shape factor for partitioning effective precipitation into runoff and infiltration based on soil wetness [-]
    real(dp), dimension(nColPars) :: infiltrationShapeFactor
    real(dp), dimension(nColPars) :: FCmin_glob !< global field capacity minimum
    real(dp), dimension(nColPars) :: FCdelta_glob !< difference between global field capacity minimum and maximum
    real(dp), dimension(nColPars) :: rootFractionCoefficient_sand !< threshold for actual ET reduction for sand
    real(dp), dimension(nColPars) :: rootFractionCoefficient_clay !< threshold for actual ET reduction for clay
  contains
    !> \copydoc mo_namelists::read_soilmoisture4
    procedure, public :: read => read_soilmoisture4 !< \see mo_namelists::read_soilmoisture4
  end type nml_soilmoisture4_t
  !> 'soilmoisture4' namelist content
  type(nml_soilmoisture4_t), public :: nml_soilmoisture4

  ! namelist /directRunoff1/ &
  !   imperviousStorageCapacity
  !
  !> \class   nml_directrunoff1_t
  !> \brief   'directrunoff1' namelist content
  type, public :: nml_directrunoff1_t
    character(13) :: name = "directrunoff1" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    real(dp), dimension(nColPars) :: imperviousStorageCapacity !< direct Runoff: Sealed Area storage capacity
  contains
    !> \copydoc mo_namelists::read_directrunoff1
    procedure, public :: read => read_directrunoff1 !< \see mo_namelists::read_directrunoff1
  end type nml_directrunoff1_t
  !> 'directrunoff1' namelist content
  type(nml_directrunoff1_t), public :: nml_directrunoff1

  ! namelist /PETminus1/  &
  !   PET_a_forest, &
  !   PET_a_impervious, &
  !   PET_a_pervious, &
  !   PET_b, &
  !   PET_c
  !
  !> \class   nml_petminus1_t
  !> \brief   'petminus1' namelist content
  !> \details PET is input, LAI driven correction
  type, public :: nml_petminus1_t
    character(9) :: name = "petminus1" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    real(dp), dimension(nColPars) :: PET_a_forest !< DSF=PET_a+PET_b*(1-exp(PET_c*LAI)) to correct PET as PET=DSF*PET
    real(dp), dimension(nColPars) :: PET_a_impervious !< DSF=PET_a+PET_b*(1-exp(PET_c*LAI)) to correct PET as PET=DSF*PET
    real(dp), dimension(nColPars) :: PET_a_pervious !< DSF=PET_a+PET_b*(1-exp(PET_c*LAI)) to correct PET as PET=DSF*PET
    real(dp), dimension(nColPars) :: PET_b !< DSF=PET_a+PET_b*(1-exp(PET_c*LAI)) to correct PET as PET=DSF*PET
    real(dp), dimension(nColPars) :: PET_c !< DSF=PET_a+PET_b*(1-exp(PET_c*LAI)) to correct PET as PET=DSF*PET
  contains
    !> \copydoc mo_namelists::read_petminus1
    procedure, public :: read => read_petminus1 !< \see mo_namelists::read_petminus1
  end type nml_petminus1_t
  !> 'petminus1' namelist content
  type(nml_petminus1_t), public :: nml_petminus1

  ! namelist /PET0/ &
  !   minCorrectionFactorPET, &
  !   maxCorrectionFactorPET, &
  !   aspectTresholdPET
  !
  !> \class   nml_pet0_t
  !> \brief   'pet0' namelist content
  !> \details PET is input, aspect driven correction
  type, public :: nml_pet0_t
    character(4) :: name = "pet0" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    real(dp), dimension(nColPars) :: minCorrectionFactorPET !< minimum factor for PET correction with aspect
    real(dp), dimension(nColPars) :: maxCorrectionFactorPET !< maximum factor for PET correction with aspect
    real(dp), dimension(nColPars) :: aspectTresholdPET !< aspect threshold for PET correction with aspect
  contains
    !> \copydoc mo_namelists::read_pet0
    procedure, public :: read => read_pet0 !< \see mo_namelists::read_pet0
  end type nml_pet0_t
  !> 'pet0' namelist content
  type(nml_pet0_t), public :: nml_pet0

  ! namelist /PET1/ &
  !   minCorrectionFactorPET, &
  !   maxCorrectionFactorPET, &
  !   aspectTresholdPET, &
  !   HargreavesSamaniCoeff
  !
  !> \class   nml_pet1_t
  !> \brief   'pet1' namelist content
  !> \details PET - Hargreaves Samani
  type, public :: nml_pet1_t
    character(4) :: name = "pet1" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    real(dp), dimension(nColPars) :: minCorrectionFactorPET !< minimum factor for PET correction with aspect
    real(dp), dimension(nColPars) :: maxCorrectionFactorPET !< maximum factor for PET correction with aspect
    real(dp), dimension(nColPars) :: aspectTresholdPET !< aspect threshold for PET correction with aspect
    real(dp), dimension(nColPars) :: HargreavesSamaniCoeff !< coefficient for Hargreaves Samani
  contains
    !> \copydoc mo_namelists::read_pet1
    procedure, public :: read => read_pet1 !< \see mo_namelists::read_pet1
  end type nml_pet1_t
  !> 'pet1' namelist content
  type(nml_pet1_t), public :: nml_pet1

  ! namelist /PET2/ &
  !   PriestleyTaylorCoeff, &
  !   PriestleyTaylorLAIcorr
  !
  !> \class   nml_pet2_t
  !> \brief   'pet2' namelist content
  !> \details PET - Priestley Taylor
  type, public :: nml_pet2_t
    character(4) :: name = "pet2" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    real(dp), dimension(nColPars) :: PriestleyTaylorCoeff !< Priestley-Taylor coefficient
    real(dp), dimension(nColPars) :: PriestleyTaylorLAIcorr !< Priestley-Taylor LAI correction factor
  contains
    !> \copydoc mo_namelists::read_pet2
    procedure, public :: read => read_pet2 !< \see mo_namelists::read_pet2
  end type nml_pet2_t
  !> 'pet2' namelist content
  type(nml_pet2_t), public :: nml_pet2

  ! namelist /PET3/ &
  !   canopyheigth_forest, &
  !   canopyheigth_impervious, &
  !   canopyheigth_pervious, &
  !   displacementheight_coeff, &
  !   roughnesslength_momentum_coeff, &
  !   roughnesslength_heat_coeff, &
  !   stomatal_resistance
  !
  !> \class   nml_pet3_t
  !> \brief   'pet3' namelist content
  !> \details PET - Penman Monteith
  type, public :: nml_pet3_t
    character(4) :: name = "pet3" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    real(dp), dimension(nColPars) :: canopyheigth_forest !< canopy height for foreset
    real(dp), dimension(nColPars) :: canopyheigth_impervious !< canopy height for impervious
    real(dp), dimension(nColPars) :: canopyheigth_pervious !< canopy height for pervious
    real(dp), dimension(nColPars) :: displacementheight_coeff !< displacement height coefficient
    real(dp), dimension(nColPars) :: roughnesslength_momentum_coeff !< roughness length momentum coefficient
    real(dp), dimension(nColPars) :: roughnesslength_heat_coeff !< roughness length heat coefficient
    real(dp), dimension(nColPars) :: stomatal_resistance !< stomatal resistance
  contains
    !> \copydoc mo_namelists::read_pet3
    procedure, public :: read => read_pet3 !< \see mo_namelists::read_pet3
  end type nml_pet3_t
  !> 'pet3' namelist content
  type(nml_pet3_t), public :: nml_pet3

  ! namelist /interflow1/ &
  !   interflowStorageCapacityFactor, &
  !   interflowRecession_slope, &
  !   fastInterflowRecession_forest, &
  !   slowInterflowRecession_Ks, &
  !   exponentSlowInterflow
  !
  !> \class   nml_interflow1_t
  !> \brief   'interflow1' namelist content
  type, public :: nml_interflow1_t
    character(10) :: name = "interflow1" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    real(dp), dimension(nColPars) :: interflowStorageCapacityFactor !< interflow storage capacity factor
    real(dp), dimension(nColPars) :: interflowRecession_slope !< multiplier for slope to derive interflow recession constant
    !> multiplier to derive fast interflow recession constant for forest
    real(dp), dimension(nColPars) :: fastInterflowRecession_forest
    !> multiplier for variability of saturated hydraulic conductivity to derive slow interflow recession constant
    real(dp), dimension(nColPars) :: slowInterflowRecession_Ks
    !> multiplier for variability of saturated hydraulic conductivity to derive slow interflow exponent
    real(dp), dimension(nColPars) :: exponentSlowInterflow
  contains
    !> \copydoc mo_namelists::read_interflow1
    procedure, public :: read => read_interflow1 !< \see mo_namelists::read_interflow1
  end type nml_interflow1_t
  !> 'interflow1' namelist content
  type(nml_interflow1_t), public :: nml_interflow1

  ! namelist /percolation1/ &
  !   rechargeCoefficient, &
  !   rechargeFactor_karstic, &
  !   gain_loss_GWreservoir_karstic
  !
  !> \class   nml_percolation1_t
  !> \brief   'percolation1' namelist content
  type, public :: nml_percolation1_t
    character(12) :: name = "percolation1" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    real(dp), dimension(nColPars) :: rechargeCoefficient !< recharge coefficient
    real(dp), dimension(nColPars) :: rechargeFactor_karstic !< recharge factor for karstic percolation
    real(dp), dimension(nColPars) :: gain_loss_GWreservoir_karstic !< gain loss in ground water reservoir for karstic
  contains
    !> \copydoc mo_namelists::read_percolation1
    procedure, public :: read => read_percolation1 !< \see mo_namelists::read_percolation1
  end type nml_percolation1_t
  !> 'percolation1' namelist content
  type(nml_percolation1_t), public :: nml_percolation1

  ! namelist /neutrons1/ &
  !   Desilets_N0, &
  !   Desilets_LW0, &
  !   Desilets_LW1
  !
  !> \class   nml_neutrons1_t
  !> \brief   'neutrons1' namelist content
  type, public :: nml_neutrons1_t
    character(9) :: name = "neutrons1" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    real(dp), dimension(nColPars) :: Desilets_N0 !< Desilets N0 parameter
    real(dp), dimension(nColPars) :: Desilets_LW0 !< Desilets LW0 parameter
    real(dp), dimension(nColPars) :: Desilets_LW1 !< Desilets LW1 parameter
  contains
    !> \copydoc mo_namelists::read_neutrons1
    procedure, public :: read => read_neutrons1 !< \see mo_namelists::read_neutrons1
  end type nml_neutrons1_t
  !> 'neutrons1' namelist content
  type(nml_neutrons1_t), public :: nml_neutrons1

  ! namelist /neutrons2/ &
  !   COSMIC_N0, &
  !   COSMIC_N1, &
  !   COSMIC_N2, &
  !   COSMIC_alpha0, &
  !   COSMIC_alpha1, &
  !   COSMIC_L30, &
  !   COSMIC_L31, &
  !   COSMIC_LW0, &
  !   COSMIC_LW1
  !
  !> \class   nml_neutrons2_t
  !> \brief   'neutrons2' namelist content
  type, public :: nml_neutrons2_t
    character(9) :: name = "neutrons2" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    real(dp), dimension(nColPars) :: COSMIC_N0 !< cosmic N0 parameter
    real(dp), dimension(nColPars) :: COSMIC_N1 !< cosmic N1 parameter
    real(dp), dimension(nColPars) :: COSMIC_N2 !< cosmic N2 parameter
    real(dp), dimension(nColPars) :: COSMIC_alpha0 !< cosmic alpha0 parameter
    real(dp), dimension(nColPars) :: COSMIC_alpha1 !< cosmic alpha1 parameter
    real(dp), dimension(nColPars) :: COSMIC_L30 !< cosmic L30 parameter
    real(dp), dimension(nColPars) :: COSMIC_L31 !< cosmic L31 parameter
    real(dp), dimension(nColPars) :: COSMIC_LW0 !< cosmic LW0 parameter
    real(dp), dimension(nColPars) :: COSMIC_LW1 !< cosmic LW1 parameter
  contains
    !> \copydoc mo_namelists::read_neutrons2
    procedure, public :: read => read_neutrons2 !< \see mo_namelists::read_neutrons2
  end type nml_neutrons2_t
  !> 'neutrons2' namelist content
  type(nml_neutrons2_t), public :: nml_neutrons2

  ! namelist /geoparameter/ &
  !   GeoParam
  !
  !> \class   nml_geoparameter_t
  !> \brief   'geoparameter' namelist content
  type, public :: nml_geoparameter_t
    character(12) :: name = "geoparameter" !< namelist name
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
    !> geological parameters (ordering according to file 'geology_classdefinition.txt')
    real(dp), dimension(maxGeoUnit, nColPars) :: GeoParam
  contains
    !> \copydoc mo_namelists::read_geoparameter
    procedure, public :: read => read_geoparameter !< \see mo_namelists::read_geoparameter
  end type nml_geoparameter_t
  !> 'geoparameter' namelist content
  type(nml_geoparameter_t), public :: nml_geoparameter

  !######## mo_mrm_read_config
  ! namelist /mainconfig_mrm/ &
  !   ALMA_convention, &
  !   filenameTotalRunoff, &
  !   varnameTotalRunoff, &
  !   gw_coupling
  !
  ! !> \class   nml_mainconfig_mhm_mrm_t
  ! !> \brief   'mainconfig_mhm_mrm' namelist content
  ! type, public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
  !   logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
  ! contains
  !   !> \copydoc mo_namelists::read_mainconfig_mhm_mrm
  !   procedure, public :: read => read_mainconfig_mhm_mrm !< \see mo_namelists::read_mainconfig_mhm_mrm
  ! end type nml_mainconfig_mhm_mrm_t
  ! !> 'mainconfig_mhm_mrm' namelist content
  ! type(nml_mainconfig_mhm_mrm_t), public :: nml_mainconfig_mhm_mrm

  ! namelist /directories_mRM/ &
  !   dir_Gauges, &
  !   dir_Total_Runoff, &
  !   dir_Bankfull_Runoff
  !
  ! !> \class   nml_mainconfig_mhm_mrm_t
  ! !> \brief   'mainconfig_mhm_mrm' namelist content
  ! type, public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
  !   logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
  ! contains
  !   !> \copydoc mo_namelists::read_mainconfig_mhm_mrm
  !   procedure, public :: read => read_mainconfig_mhm_mrm !< \see mo_namelists::read_mainconfig_mhm_mrm
  ! end type nml_mainconfig_mhm_mrm_t
  ! !> 'mainconfig_mhm_mrm' namelist content
  ! type(nml_mainconfig_mhm_mrm_t), public :: nml_mainconfig_mhm_mrm

  ! namelist /evaluation_gauges/ &
  !   nGaugesTotal, &
  !   NoGauges_domain, &
  !   Gauge_id, &
  !   gauge_filename
  !
  ! !> \class   nml_mainconfig_mhm_mrm_t
  ! !> \brief   'mainconfig_mhm_mrm' namelist content
  ! type, public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
  !   logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
  ! contains
  !   !> \copydoc mo_namelists::read_mainconfig_mhm_mrm
  !   procedure, public :: read => read_mainconfig_mhm_mrm !< \see mo_namelists::read_mainconfig_mhm_mrm
  ! end type nml_mainconfig_mhm_mrm_t
  ! !> 'mainconfig_mhm_mrm' namelist content
  ! type(nml_mainconfig_mhm_mrm_t), public :: nml_mainconfig_mhm_mrm

  ! namelist /inflow_gauges/ &
  !   nInflowGaugesTotal, &
  !   NoInflowGauges_domain, &
  !   InflowGauge_id, &
  !   InflowGauge_filename, &
  !   InflowGauge_Headwater
  !
  ! !> \class   nml_mainconfig_mhm_mrm_t
  ! !> \brief   'mainconfig_mhm_mrm' namelist content
  ! type, public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
  !   logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
  ! contains
  !   !> \copydoc mo_namelists::read_mainconfig_mhm_mrm
  !   procedure, public :: read => read_mainconfig_mhm_mrm !< \see mo_namelists::read_mainconfig_mhm_mrm
  ! end type nml_mainconfig_mhm_mrm_t
  ! !> 'mainconfig_mhm_mrm' namelist content
  ! type(nml_mainconfig_mhm_mrm_t), public :: nml_mainconfig_mhm_mrm

  ! namelist /NLoutputResults/ &
  !   output_deflate_level_mrm, &
  !   output_double_precision_mrm, &
  !   timeStep_model_outputs_mrm, &
  !   outputFlxState_mrm
  !
  ! !> \class   nml_mainconfig_mhm_mrm_t
  ! !> \brief   'mainconfig_mhm_mrm' namelist content
  ! type, public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
  !   logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
  ! contains
  !   !> \copydoc mo_namelists::read_mainconfig_mhm_mrm
  !   procedure, public :: read => read_mainconfig_mhm_mrm !< \see mo_namelists::read_mainconfig_mhm_mrm
  ! end type nml_mainconfig_mhm_mrm_t
  ! !> 'mainconfig_mhm_mrm' namelist content
  ! type(nml_mainconfig_mhm_mrm_t), public :: nml_mainconfig_mhm_mrm

  ! namelist /routing1/ &
  !   muskingumTravelTime_constant, &
  !   muskingumTravelTime_riverLength, &
  !   muskingumTravelTime_riverSlope, &
  !   muskingumTravelTime_impervious, &
  !   muskingumAttenuation_riverSlope
  !
  ! !> \class   nml_mainconfig_mhm_mrm_t
  ! !> \brief   'mainconfig_mhm_mrm' namelist content
  ! type, public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
  !   logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
  ! contains
  !   !> \copydoc mo_namelists::read_mainconfig_mhm_mrm
  !   procedure, public :: read => read_mainconfig_mhm_mrm !< \see mo_namelists::read_mainconfig_mhm_mrm
  ! end type nml_mainconfig_mhm_mrm_t
  ! !> 'mainconfig_mhm_mrm' namelist content
  ! type(nml_mainconfig_mhm_mrm_t), public :: nml_mainconfig_mhm_mrm

  ! namelist /routing2/ &
  !   streamflow_celerity
  !
  ! !> \class   nml_mainconfig_mhm_mrm_t
  ! !> \brief   'mainconfig_mhm_mrm' namelist content
  ! type, public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
  !   logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
  ! contains
  !   !> \copydoc mo_namelists::read_mainconfig_mhm_mrm
  !   procedure, public :: read => read_mainconfig_mhm_mrm !< \see mo_namelists::read_mainconfig_mhm_mrm
  ! end type nml_mainconfig_mhm_mrm_t
  ! !> 'mainconfig_mhm_mrm' namelist content
  ! type(nml_mainconfig_mhm_mrm_t), public :: nml_mainconfig_mhm_mrm

  ! namelist /routing3/ &
  !   slope_factor
  !
  ! !> \class   nml_mainconfig_mhm_mrm_t
  ! !> \brief   'mainconfig_mhm_mrm' namelist content
  ! type, public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
  !   logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
  ! contains
  !   !> \copydoc mo_namelists::read_mainconfig_mhm_mrm
  !   procedure, public :: read => read_mainconfig_mhm_mrm !< \see mo_namelists::read_mainconfig_mhm_mrm
  ! end type nml_mainconfig_mhm_mrm_t
  ! !> 'mainconfig_mhm_mrm' namelist content
  ! type(nml_mainconfig_mhm_mrm_t), public :: nml_mainconfig_mhm_mrm

  !######## mo_mrm_riv_temp_class
  ! namelist /config_riv_temp/ &
  !   albedo_water, &
  !   pt_a_water, &
  !   emissivity_water, &
  !   turb_heat_ex_coeff, &
  !   max_iter, &
  !   delta_iter, &
  !   step_iter, &
  !   riv_widths_file, &
  !   riv_widths_name, &
  !   dir_riv_widths
  !
  ! !> \class   nml_mainconfig_mhm_mrm_t
  ! !> \brief   'mainconfig_mhm_mrm' namelist content
  ! type, public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
  !   logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
  ! contains
  !   !> \copydoc mo_namelists::read_mainconfig_mhm_mrm
  !   procedure, public :: read => read_mainconfig_mhm_mrm !< \see mo_namelists::read_mainconfig_mhm_mrm
  ! end type nml_mainconfig_mhm_mrm_t
  ! !> 'mainconfig_mhm_mrm' namelist content
  ! type(nml_mainconfig_mhm_mrm_t), public :: nml_mainconfig_mhm_mrm

contains

  !> \brief Read 'project_description' namelist content.
  subroutine read_project_description(self, file, unit)
    implicit none
    class(nml_project_description_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    character(1024) :: project_details !< project including funding instituion., PI, etc.
    character(1024) :: setup_description !< any specific description of simulation
    character(1024) :: simulation_type !< e.g. seasonal forecast, climate projection, ...
    character(256) :: Conventions !< convention used for dataset
    character(1024) :: contact !< contact details, incl. PI name
    character(1024) :: mHM_details !< developing institution, specific mHM revision
    character(1024) :: history !< details on version/creation date

    namelist /project_description/ &
      project_details, &
      setup_description, &
      simulation_type, &
      Conventions, &
      contact, &
      mHM_details, &
      history

    if ( self%read_from_file ) then
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=project_description)
      call close_nml(unit)
      self%project_details = project_details
      self%setup_description = setup_description
      self%simulation_type = simulation_type
      self%Conventions = Conventions
      self%contact = contact
      self%mHM_details = mHM_details
      self%history = history
      self%read_from_file = .false.
    end if
  end subroutine read_project_description

  !> \brief Read 'directories_general' namelist content.
  subroutine read_directories_general(self, file, unit)
    implicit none
    class(nml_directories_general_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    character(256) :: dirConfigOut !< directory for config file output
    character(256) :: dirCommonFiles !< directory where common input files should be located
    character(256), dimension(maxNoDomains) :: mhm_file_RestartOut !< Directory where mhm output of restart is written
    character(256), dimension(maxNoDomains) :: mrm_file_RestartOut !< Directory where mrm output of restart is written
    character(256), dimension(maxNoDomains) :: dir_Morpho !< Directory where morphological files are located
    character(256), dimension(maxNoDomains) :: dir_LCover !< Directory where land cover files are located
    character(256), dimension(maxNoDomains) :: dir_Out !< Directory where output is written to
    character(256), dimension(maxNoDomains) :: file_LatLon !< Directory where the Lat Lon Files are located

    namelist /directories_general/ &
      dirConfigOut, &
      dirCommonFiles, &
      dir_Morpho, &
      dir_LCover, &
      dir_Out, &
      mhm_file_RestartOut, &
      mrm_file_RestartOut, &
      file_LatLon

    if ( self%read_from_file ) then
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=directories_general)
      call close_nml(unit)
      self%dirConfigOut = dirConfigOut
      self%dirCommonFiles = dirCommonFiles
      self%dir_Morpho = dir_Morpho
      self%dir_LCover = dir_LCover
      self%dir_Out = dir_Out
      self%mhm_file_RestartOut = mhm_file_RestartOut
      self%mrm_file_RestartOut = mrm_file_RestartOut
      self%file_LatLon = file_LatLon
      self%read_from_file = .false.
    end if
  end subroutine read_directories_general

  !> \brief Read 'mainconfig' namelist content.
  subroutine read_mainconfig(self, file, unit)
    implicit none
    class(nml_mainconfig_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    integer(i4) :: iFlag_cordinate_sys !< options model for the run cordinate system
    real(dp), dimension(maxNoDomains) :: resolution_Hydrology !< [m or degree] resolution of hydrology - Level 1
    integer(i4) :: nDomains !< number of domains
    integer(i4), dimension(maxNoDomains) :: L0Domain !< specify same index for domains to share L0_data to save memory
    logical :: write_restart !< flag to write restart
    integer(i4), dimension(maxNoDomains) :: read_opt_domain_data !< read domain specific optional data

    namelist /mainconfig/ &
      iFlag_cordinate_sys, &
      resolution_Hydrology, &
      nDomains, &
      L0Domain, &
      write_restart, &
      read_opt_domain_data

    if ( self%read_from_file ) then
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=mainconfig)
      call close_nml(unit)
      self%iFlag_cordinate_sys = iFlag_cordinate_sys
      self%resolution_Hydrology = resolution_Hydrology
      self%nDomains = nDomains
      self%L0Domain = L0Domain
      self%write_restart = write_restart
      self%read_opt_domain_data = read_opt_domain_data
      self%read_from_file = .false.
    end if
  end subroutine read_mainconfig

  !> \brief Read 'processSelection' namelist content.
  subroutine read_processSelection(self, file, unit)
    implicit none
    class(nml_processSelection_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    integer(i4), dimension(nProcesses) :: processCase !< ! Choosen process description number

    namelist /processSelection/ &
      processCase

    if ( self%read_from_file ) then
      ! init the processCase matrix to 0 to be backward compatible
      ! if cases were added later (then there would be no values if not init here)
      processCase = 0_i4
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=processSelection)
      call close_nml(unit)
      self%processCase = processCase
      self%read_from_file = .false.
    end if
  end subroutine read_processSelection

  !> \brief Read 'LCover' namelist content.
  subroutine read_LCover(self, file, unit)
    implicit none
    class(nml_LCover_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    integer(i4) :: nLCoverScene !< Number of land cover scene (lcs)
    integer(i4), dimension(maxNLCovers) :: LCoverYearStart !< starting year LCover
    integer(i4), dimension(maxNLCovers) :: LCoverYearEnd !< ending year LCover
    character(256), dimension(maxNLCovers) :: LCoverfName !< filename of Lcover file

    namelist /LCover/ &
      nLcoverScene, &
      LCoverYearStart, &
      LCoverYearEnd, &
      LCoverfName

    if ( self%read_from_file ) then
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=LCover)
      call close_nml(unit)
      self%nLcoverScene = nLcoverScene
      self%LCoverYearStart = LCoverYearStart
      self%LCoverYearEnd = LCoverYearEnd
      self%LCoverfName = LCoverfName
      self%read_from_file = .false.
    end if
  end subroutine read_LCover

  !> \brief Read 'mainconfig_mhm_mrm' namelist content.
  subroutine read_mainconfig_mhm_mrm(self, file, unit)
    implicit none
    class(nml_mainconfig_mhm_mrm_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    integer(i4) :: timeStep !< [h] simulation time step (= TS) in [h] either 1, 2, 3, 4, 6, 12 or 24
    real(dp), dimension(maxNoDomains) :: resolution_Routing !< resolution of Level-11 discharge routing [m or degree] per domain
    logical :: optimize !< Optimization (.true.) or Evaluation run (.false.)
    logical :: optimize_restart !< Optimization will be restarted from mo_<opti_method>.restart file (.true.)
    integer(i4) :: opti_method !< Optimization algorithm: 1 - DDS; 2 - Simulated Annealing; 3 - SCE
    integer(i4) :: opti_function !< Objective function
    logical :: read_restart !< flag for reading restart output
    logical :: mrm_read_river_network !< flag to read the river network for mRM (read_restart = .True. forces .True.)
    logical :: read_old_style_restart_bounds !< flag to use an old-style restart file created by mhm<=v5.11
    character(256), dimension(maxNoDomains) :: mhm_file_RestartIn !< mhm restart file paths
    character(256), dimension(maxNoDomains) :: mrm_file_RestartIn !< mrm restart file paths

    namelist /mainconfig_mhm_mrm/ &
      timestep, &
      resolution_Routing, &
      optimize, &
      optimize_restart, &
      opti_method, &
      opti_function, &
      read_restart, &
      mrm_read_river_network, &
      read_old_style_restart_bounds, &
      mhm_file_RestartIn, &
      mrm_file_RestartIn

    if ( self%read_from_file ) then
      ! set default values for optional arguments
      mrm_read_river_network = .false.
      read_old_style_restart_bounds = .false.
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=mainconfig_mhm_mrm)
      call close_nml(unit)
      self%timestep = timestep
      self%resolution_Routing = resolution_Routing
      self%optimize = optimize
      self%optimize_restart = optimize_restart
      self%opti_method = opti_method
      self%opti_function = opti_function
      self%read_restart = read_restart
      self%mrm_read_river_network = mrm_read_river_network
      self%read_old_style_restart_bounds = read_old_style_restart_bounds
      self%mhm_file_RestartIn = mhm_file_RestartIn
      self%mrm_file_RestartIn = mrm_file_RestartIn
      self%read_from_file = .false.
    end if
  end subroutine read_mainconfig_mhm_mrm

  !> \brief Read 'Optimization' namelist content.
  subroutine read_Optimization(self, file, unit)
    implicit none
    class(nml_Optimization_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    integer(i4) :: nIterations !< number of iterations for optimization
    integer(i8) :: seed !< seed used for optimization, default: -9 --> system time
    real(dp) :: dds_r !< DDS: perturbation rate, default: 0.2
    real(dp) :: sa_temp !< SA:  initial temperature, default: -9.0 --> estimated
    integer(i4) :: sce_ngs !< SCE: # of complexes, default: 2
    integer(i4) :: sce_npg !< SCE: # of points per complex,default: -9 --> 2n+1
    integer(i4) :: sce_nps !< SCE: # of points per subcomplex,default: -9 --> n+1
    logical :: mcmc_opti !< MCMC: Optimization (.true.) or only parameter uncertainty (.false.)
    real(dp), dimension(nerror_model) :: mcmc_error_params !< error model para (mcmc_opti=.false.) e.g. for opti_function=8: .01, .3

    namelist /Optimization/ &
      nIterations, &
      seed, &
      dds_r, &
      sa_temp, &
      sce_ngs, &
      sce_npg, &
      sce_nps, &
      mcmc_opti, &
      mcmc_error_params

    if ( self%read_from_file ) then
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=Optimization)
      call close_nml(unit)
      self%nIterations = nIterations
      self%seed = seed
      self%dds_r = dds_r
      self%sa_temp = sa_temp
      self%sce_ngs = sce_ngs
      self%sce_npg = sce_npg
      self%sce_nps = sce_nps
      self%mcmc_opti = mcmc_opti
      self%mcmc_error_params = mcmc_error_params
      self%read_from_file = .false.
    end if
  end subroutine read_Optimization

  !> \brief Read 'time_periods' namelist content.
  subroutine read_time_periods(self, file, unit)
    implicit none
    class(nml_time_periods_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    integer(i4), dimension(maxNoDomains) :: warming_Days !< number of days for warm up period
    type(period), dimension(maxNoDomains) :: eval_Per !< time period for model evaluation

    namelist /time_periods/ &
      warming_Days, &
      eval_Per

      if ( self%read_from_file ) then
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=time_periods)
      call close_nml(unit)
      self%warming_Days = warming_Days
      self%eval_Per = eval_Per
      self%read_from_file = .false.
    end if
  end subroutine read_time_periods

  !> \brief Read 'directories_mHM' namelist content.
  subroutine read_directories_mHM(self, file, unit)
    implicit none
    class(nml_directories_mHM_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    character(256) :: inputFormat_meteo_forcings !< format of meteo input data (nc)
    character(256), dimension(maxNoDomains) :: dir_Precipitation !< Directory where precipitation files are located
    character(256), dimension(maxNoDomains) :: dir_Temperature !< Directory where temperature files are located
    character(256), dimension(maxNoDomains) :: dir_ReferenceET !< Directory where reference-ET files are located
    character(256), dimension(maxNoDomains) :: dir_MinTemperature !< Directory where minimum temp. files are located
    character(256), dimension(maxNoDomains) :: dir_MaxTemperature !< Directory where maximum temp. files are located
    character(256), dimension(maxNoDomains) :: dir_absVapPressure !< Directory where abs. vap. pressure files are located
    character(256), dimension(maxNoDomains) :: dir_windspeed !< Directory where windspeed files are located
    character(256), dimension(maxNoDomains) :: dir_NetRadiation !< Directory where abs. vap. pressure files are located
    character(256), dimension(maxNoDomains) :: dir_Radiation !< riv-temp related: directory of (long/short-wave)radiation
    integer(i4), dimension(maxNoDomains) :: time_step_model_inputs !< frequency for reading meteo input

    namelist /directories_mHM/ &
      inputFormat_meteo_forcings, &
      dir_Precipitation, &
      dir_Temperature, &
      dir_ReferenceET, &
      dir_MinTemperature, &
      dir_MaxTemperature, &
      dir_absVapPressure, &
      dir_windspeed, &
      dir_NetRadiation, &
      dir_Radiation, &
      time_step_model_inputs

    if ( self%read_from_file ) then
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=directories_mHM)
      call close_nml(unit)
      self%inputFormat_meteo_forcings = inputFormat_meteo_forcings
      self%dir_Precipitation = dir_Precipitation
      self%dir_Temperature = dir_Temperature
      self%dir_ReferenceET = dir_ReferenceET
      self%dir_MinTemperature = dir_MinTemperature
      self%dir_MaxTemperature = dir_MaxTemperature
      self%dir_absVapPressure = dir_absVapPressure
      self%dir_windspeed = dir_windspeed
      self%dir_NetRadiation = dir_NetRadiation
      self%dir_Radiation = dir_Radiation
      self%time_step_model_inputs = time_step_model_inputs
      self%read_from_file = .false.
    end if
  end subroutine read_directories_mHM

  !> \brief Read 'optional_data' namelist content.
  subroutine read_optional_data(self, file, unit)
    implicit none
    class(nml_optional_data_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    integer(i4) :: nSoilHorizons_sm_input !< No. of mhm soil horizons equivalent to sm input
    character(256), dimension(maxNoDomains) :: dir_soil_moisture !< soil moisture input
    character(256), dimension(maxNoDomains) :: dir_neutrons !< ground albedo neutron input
    character(256), dimension(maxNoDomains) :: dir_evapotranspiration !< evapotranspiration input
    character(256), dimension(maxNoDomains) :: dir_TWS !< tws input
    integer(i4) :: timeStep_sm_input !< time step of optional data: sm
    integer(i4) :: timeStep_neutrons_input !< time step of optional data: neutrons
    integer(i4) :: timeStep_et_input !< time step of optional data: et
    integer(i4) :: timeStep_tws_input !< time step of optional data: tws

    namelist /optional_data/ &
      nSoilHorizons_sm_input, &
      dir_soil_moisture, &
      dir_neutrons, &
      dir_evapotranspiration, &
      dir_TWS, &
      timeStep_sm_input, &
      timeStep_neutrons_input, &
      timeStep_et_input, &
      timeStep_tws_input

    if ( self%read_from_file ) then
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=optional_data)
      call close_nml(unit)
      self%nSoilHorizons_sm_input = nSoilHorizons_sm_input
      self%dir_soil_moisture = dir_soil_moisture
      self%dir_neutrons = dir_neutrons
      self%dir_evapotranspiration = dir_evapotranspiration
      self%dir_TWS = dir_TWS
      self%timeStep_sm_input = timeStep_sm_input
      self%timeStep_neutrons_input = timeStep_neutrons_input
      self%timeStep_et_input = timeStep_et_input
      self%timeStep_tws_input = timeStep_tws_input
      self%read_from_file = .false.
    end if
  end subroutine read_optional_data

  !> \brief Read 'panEvapo' namelist content.
  subroutine read_panEvapo(self, file, unit)
    implicit none
    class(nml_panEvapo_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    real(dp), dimension(int(YearMonths, i4)) :: evap_coeff !< [-] Evap. coef. for free-water surfaces

    namelist /panEvapo/ &
      evap_coeff

    if ( self%read_from_file ) then
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=panEvapo)
      call close_nml(unit)
      self%evap_coeff = evap_coeff
      self%read_from_file = .false.
    end if
  end subroutine read_panEvapo

  !> \brief Read 'nightDayRatio' namelist content.
  subroutine read_nightDayRatio(self, file, unit)
    implicit none
    class(nml_nightDayRatio_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    logical :: read_meteo_weights !< read weights for meteo data
    real(dp), dimension(int(YearMonths, i4)) :: fnight_prec !< [-] Night ratio precipitation < 1
    real(dp), dimension(int(YearMonths, i4)) :: fnight_pet !< [-] Night ratio PET  < 1
    real(dp), dimension(int(YearMonths, i4)) :: fnight_temp !< [-] Night factor mean temp
    real(dp), dimension(int(YearMonths, i4)) :: fnight_ssrd !< [-] Night factor short-wave rad.
    real(dp), dimension(int(YearMonths, i4)) :: fnight_strd !< [-] Night factor long-wave rad.

    namelist /nightDayRatio/ &
      read_meteo_weights, &
      fnight_prec, &
      fnight_pet, &
      fnight_temp, &
      fnight_ssrd, &
      fnight_strd

    if ( self%read_from_file ) then
      ! default values for long/shortwave rad.
      fnight_ssrd = 0.0_dp
      fnight_strd = 0.45_dp
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=nightDayRatio)
      call close_nml(unit)
      self%read_meteo_weights = read_meteo_weights
      self%fnight_prec = fnight_prec
      self%fnight_pet = fnight_pet
      self%fnight_temp = fnight_temp
      self%fnight_ssrd = fnight_ssrd
      self%fnight_strd = fnight_strd
      self%read_from_file = .false.
    end if
  end subroutine read_nightDayRatio

  !> \brief Read 'NLoutputResults' namelist content.
  subroutine read_NLoutputResults(self, file, unit)
    implicit none
    class(nml_NLoutputResults_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    integer(i4) :: output_deflate_level !< deflate level in nc files
    logical :: output_double_precision !< output precision in nc files
    integer(i4) :: timeStep_model_outputs !< timestep for writing model outputs
    logical, dimension(nOutFlxState) :: outputFlxState !< Define model outputs see "mhm_outputs.nml"

    namelist /NLoutputResults/ &
      output_deflate_level, &
      output_double_precision, &
      timeStep_model_outputs, &
      outputFlxState

    if ( self%read_from_file ) then
      ! default values
      output_deflate_level = 6
      output_double_precision = .true.
      outputFlxState = .FALSE.
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=NLoutputResults)
      call close_nml(unit)
      self%output_deflate_level = output_deflate_level
      self%output_double_precision = output_double_precision
      self%timeStep_model_outputs = timeStep_model_outputs
      self%outputFlxState = outputFlxState
      self%read_from_file = .false.
    end if
  end subroutine read_NLoutputResults

  !> \brief Read 'BFI_inputs' namelist content.
  subroutine read_BFI_inputs(self, file, unit)
    implicit none
    class(nml_BFI_inputs_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    logical :: BFI_calc !< calculate observed BFI from gauges with Eckhardt filter
    real(dp), dimension(maxNoDomains) :: BFI_obs !< given base-flow index per domain

    namelist /BFI_inputs/ &
      BFI_calc, &
      BFI_obs

    if ( self%read_from_file ) then
      BFI_calc = .false. ! default value
      BFI_obs = -1.0_dp  ! negative value to flag missing values
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=BFI_inputs)
      call close_nml(unit)
      self%BFI_calc = BFI_calc
      self%BFI_obs = BFI_obs
      self%read_from_file = .false.
    end if
  end subroutine read_BFI_inputs

  !> \brief Read 'directories_mpr' namelist content.
  subroutine read_directories_mpr(self, file, unit)
    implicit none
    class(nml_directories_mpr_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    character(256), dimension(maxNoDomains) :: dir_gridded_LAI !< directory of gridded LAI data, used when timeStep_LAI_input<0

    namelist /directories_mpr/ &
      dir_gridded_LAI

    if ( self%read_from_file ) then
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=directories_mpr)
      call close_nml(unit)
      self%dir_gridded_LAI = dir_gridded_LAI
      self%read_from_file = .false.
    end if
  end subroutine read_directories_mpr

  !> \brief Read 'soildata' namelist content.
  subroutine read_soildata(self, file, unit)
    implicit none
    class(nml_soildata_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    integer(i4) :: iFlag_soilDB !< options to handle different soil databases
    real(dp) :: tillageDepth !< [mm] Soil depth down to which organic
    integer(i4) :: nSoilHorizons_mHM !< Number of horizons to model
    real(dp), dimension(maxNoSoilHorizons) :: soil_Depth !< depth of the single horizons

    namelist /soildata/ &
      iFlag_soilDB, &
      tillageDepth, &
      nSoilHorizons_mHM, &
      soil_Depth

    if ( self%read_from_file ) then
      soil_Depth = 0.0_dp ! default soil depth
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=soildata)
      call close_nml(unit)
      self%iFlag_soilDB = iFlag_soilDB
      self%tillageDepth = tillageDepth
      self%nSoilHorizons_mHM = nSoilHorizons_mHM
      self%soil_Depth = soil_Depth
      self%read_from_file = .false.
    end if
  end subroutine read_soildata

  !> \brief Read 'lai_data_information' namelist content.
  subroutine read_lai_data_information(self, file, unit)
    implicit none
    class(nml_lai_data_information_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    character(256) :: inputFormat_gridded_LAI !< format of gridded LAI data (nc only)
    integer(i4) :: timeStep_LAI_input !< time step of gridded LAI input

    namelist /lai_data_information/ &
      inputFormat_gridded_LAI, &
      timeStep_LAI_input

    if ( self%read_from_file ) then
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=lai_data_information)
      call close_nml(unit)
      self%inputFormat_gridded_LAI = inputFormat_gridded_LAI
      self%timeStep_LAI_input = timeStep_LAI_input
      self%read_from_file = .false.
    end if
  end subroutine read_lai_data_information

  !> \brief Read 'lcover_mpr' namelist content.
  subroutine read_lcover_mpr(self, file, unit)
    implicit none
    class(nml_lcover_mpr_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    real(dp) :: fracSealed_cityArea !< fraction of area within city assumed to be perfectly sealed [0-1]

    namelist /lcover_mpr/ &
      fracSealed_cityArea

    if ( self%read_from_file ) then
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=lcover_mpr)
      call close_nml(unit)
      self%fracSealed_cityArea = fracSealed_cityArea
      self%read_from_file = .false.
    end if
  end subroutine read_lcover_mpr

  !> \brief Read 'interception1' namelist content.
  subroutine read_interception1(self, file, unit)
    implicit none
    class(nml_interception1_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    real(dp), dimension(nColPars) :: canopyInterceptionFactor !< multiplier to relate LAI to interception storage [-]

    namelist /interception1/ &
      canopyInterceptionFactor

    if ( self%read_from_file ) then
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=interception1)
      call close_nml(unit)
      self%canopyInterceptionFactor = canopyInterceptionFactor
      self%read_from_file = .false.
    end if
  end subroutine read_interception1

  !> \brief Read 'snow1' namelist content.
  subroutine read_snow1(self, file, unit)
    implicit none
    class(nml_snow1_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    real(dp), dimension(nColPars) :: snowTreshholdTemperature !< Threshold for rain/snow partitioning [degC]
    real(dp), dimension(nColPars) :: degreeDayFactor_forest !< forest: deg day factors to determine melting flux [m degC-1]
    real(dp), dimension(nColPars) :: degreeDayFactor_impervious !< impervious: deg day factors to determine melting flux [m degC-1]
    real(dp), dimension(nColPars) :: degreeDayFactor_pervious !< pervious: deg day factors to determine melting flux [m degC-1]
    real(dp), dimension(nColPars) :: increaseDegreeDayFactorByPrecip !< increase of deg day factor in case of precipitation [degC-1]
    real(dp), dimension(nColPars) :: maxDegreeDayFactor_forest !< forest: maximum values for degree day factor [m degC-1]
    real(dp), dimension(nColPars) :: maxDegreeDayFactor_impervious !< impervious: maximum values for degree day factor [m degC-1]
    real(dp), dimension(nColPars) :: maxDegreeDayFactor_pervious !< pervious: maximum values for degree day factor [m degC-1]

    namelist /snow1/ &
      snowTreshholdTemperature, &
      degreeDayFactor_forest, &
      degreeDayFactor_impervious, &
      degreeDayFactor_pervious, &
      increaseDegreeDayFactorByPrecip, &
      maxDegreeDayFactor_forest, &
      maxDegreeDayFactor_impervious, &
      maxDegreeDayFactor_pervious

    if ( self%read_from_file ) then
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=snow1)
      call close_nml(unit)
      self%snowTreshholdTemperature = snowTreshholdTemperature
      self%degreeDayFactor_forest = degreeDayFactor_forest
      self%degreeDayFactor_impervious = degreeDayFactor_impervious
      self%degreeDayFactor_pervious = degreeDayFactor_pervious
      self%increaseDegreeDayFactorByPrecip = increaseDegreeDayFactorByPrecip
      self%maxDegreeDayFactor_forest = maxDegreeDayFactor_forest
      self%maxDegreeDayFactor_impervious = maxDegreeDayFactor_impervious
      self%maxDegreeDayFactor_pervious = maxDegreeDayFactor_pervious
      self%read_from_file = .false.
    end if
  end subroutine read_snow1

  !> \brief Read 'soilmoisture1' namelist content.
  subroutine read_soilmoisture1(self, file, unit)
    implicit none
    class(nml_soilmoisture1_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    real(dp), dimension(nColPars) :: orgMatterContent_forest !< organic matter content [%] for forest
    real(dp), dimension(nColPars) :: orgMatterContent_impervious !< organic matter content [%] for impervious
    real(dp), dimension(nColPars) :: orgMatterContent_pervious !< organic matter content [%] for pervious
    !> Zacharias PTF parameters below 66.5 % sand content (Zacharias et al., 2007, doi:10.2136/sssaj2006.0098)
    real(dp), dimension(nColPars) :: PTF_lower66_5_constant
    real(dp), dimension(nColPars) :: PTF_lower66_5_clay !< multiplier for clay constant (see PTF_lower66_5_constant)
    real(dp), dimension(nColPars) :: PTF_lower66_5_Db !< multiplier for mineral bulk density (see PTF_lower66_5_constant)
    !> Zacharias PTF parameters above 66.5 % sand content (Zacharias et al., 2007, doi:10.2136/sssaj2006.0098)
    real(dp), dimension(nColPars) :: PTF_higher66_5_constant
    real(dp), dimension(nColPars) :: PTF_higher66_5_clay !< multiplier for clay constant (see PTF_higher66_5_constant)
    real(dp), dimension(nColPars) :: PTF_higher66_5_Db !< multiplier for mineral bulk density (see PTF_higher66_5_constant)
    !> PTF parameters for saturated hydraulic conductivity after Cosby et al. (1984)
    real(dp), dimension(nColPars) :: PTF_Ks_constant
    real(dp), dimension(nColPars) :: PTF_Ks_sand !< multiplier for sand (see PTF_Ks_constant)
    real(dp), dimension(nColPars) :: PTF_Ks_clay !< multiplier for clay (see PTF_Ks_constant)
    real(dp), dimension(nColPars) :: PTF_Ks_curveSlope !< unit conversion factor from inch/h to cm/d
    !> shape factor for root distribution with depth, which follows an exponential function [-] for forest
    real(dp), dimension(nColPars) :: rootFractionCoefficient_forest
    !> shape factor for root distribution with depth, which follows an exponential function [-] for impervious
    real(dp), dimension(nColPars) :: rootFractionCoefficient_impervious
    !> shape factor for root distribution with depth, which follows an exponential function [-] for pervious
    real(dp), dimension(nColPars) :: rootFractionCoefficient_pervious
    !> shape factor for partitioning effective precipitation into runoff and infiltration based on soil wetness [-]
    real(dp), dimension(nColPars) :: infiltrationShapeFactor

    namelist /soilmoisture1/ &
      orgMatterContent_forest, &
      orgMatterContent_impervious, &
      orgMatterContent_pervious, &
      PTF_lower66_5_constant, &
      PTF_lower66_5_clay, &
      PTF_lower66_5_Db, &
      PTF_higher66_5_constant, &
      PTF_higher66_5_clay, &
      PTF_higher66_5_Db, &
      PTF_Ks_constant, &
      PTF_Ks_sand, &
      PTF_Ks_clay, &
      PTF_Ks_curveSlope, &
      rootFractionCoefficient_forest, &
      rootFractionCoefficient_impervious, &
      rootFractionCoefficient_pervious, &
      infiltrationShapeFactor

    if ( self%read_from_file ) then
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=soilmoisture1)
      call close_nml(unit)
      self%orgMatterContent_forest = orgMatterContent_forest
      self%orgMatterContent_impervious = orgMatterContent_impervious
      self%orgMatterContent_pervious = orgMatterContent_pervious
      self%PTF_lower66_5_constant = PTF_lower66_5_constant
      self%PTF_lower66_5_clay = PTF_lower66_5_clay
      self%PTF_lower66_5_Db = PTF_lower66_5_Db
      self%PTF_higher66_5_constant = PTF_higher66_5_constant
      self%PTF_higher66_5_clay = PTF_higher66_5_clay
      self%PTF_higher66_5_Db = PTF_higher66_5_Db
      self%PTF_Ks_constant = PTF_Ks_constant
      self%PTF_Ks_sand = PTF_Ks_sand
      self%PTF_Ks_clay = PTF_Ks_clay
      self%PTF_Ks_curveSlope = PTF_Ks_curveSlope
      self%rootFractionCoefficient_forest = rootFractionCoefficient_forest
      self%rootFractionCoefficient_impervious = rootFractionCoefficient_impervious
      self%rootFractionCoefficient_pervious = rootFractionCoefficient_pervious
      self%infiltrationShapeFactor = infiltrationShapeFactor
      self%read_from_file = .false.
    end if
  end subroutine read_soilmoisture1

  !> \brief Read 'soilmoisture2' namelist content.
  subroutine read_soilmoisture2(self, file, unit)
    implicit none
    class(nml_soilmoisture2_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    real(dp), dimension(nColPars) :: orgMatterContent_forest !< organic matter content [%] for forest
    real(dp), dimension(nColPars) :: orgMatterContent_impervious !< organic matter content [%] for impervious
    real(dp), dimension(nColPars) :: orgMatterContent_pervious !< organic matter content [%] for pervious
    !> Zacharias PTF parameters below 66.5 % sand content (Zacharias et al., 2007, doi:10.2136/sssaj2006.0098)
    real(dp), dimension(nColPars) :: PTF_lower66_5_constant
    real(dp), dimension(nColPars) :: PTF_lower66_5_clay !< multiplier for clay constant (see PTF_lower66_5_constant)
    real(dp), dimension(nColPars) :: PTF_lower66_5_Db !< multiplier for mineral bulk density (see PTF_lower66_5_constant)
    !> Zacharias PTF parameters above 66.5 % sand content (Zacharias et al., 2007, doi:10.2136/sssaj2006.0098)
    real(dp), dimension(nColPars) :: PTF_higher66_5_constant
    real(dp), dimension(nColPars) :: PTF_higher66_5_clay !< multiplier for clay constant (see PTF_higher66_5_constant)
    real(dp), dimension(nColPars) :: PTF_higher66_5_Db !< multiplier for mineral bulk density (see PTF_higher66_5_constant)
    !> PTF parameters for saturated hydraulic conductivity after Cosby et al. (1984)
    real(dp), dimension(nColPars) :: PTF_Ks_constant
    real(dp), dimension(nColPars) :: PTF_Ks_sand !< multiplier for sand (see PTF_Ks_constant)
    real(dp), dimension(nColPars) :: PTF_Ks_clay !< multiplier for clay (see PTF_Ks_constant)
    real(dp), dimension(nColPars) :: PTF_Ks_curveSlope !< unit conversion factor from inch/h to cm/d
    !> shape factor for root distribution with depth, which follows an exponential function [-] for forest
    real(dp), dimension(nColPars) :: rootFractionCoefficient_forest
    !> shape factor for root distribution with depth, which follows an exponential function [-] for impervious
    real(dp), dimension(nColPars) :: rootFractionCoefficient_impervious
    !> shape factor for root distribution with depth, which follows an exponential function [-] for pervious
    real(dp), dimension(nColPars) :: rootFractionCoefficient_pervious
    !> shape factor for partitioning effective precipitation into runoff and infiltration based on soil wetness [-]
    real(dp), dimension(nColPars) :: infiltrationShapeFactor
    real(dp), dimension(nColPars) :: jarvis_sm_threshold_c1 !< soil moisture threshod for jarvis model

    namelist /soilmoisture2/ &
      orgMatterContent_forest, &
      orgMatterContent_impervious, &
      orgMatterContent_pervious, &
      PTF_lower66_5_constant, &
      PTF_lower66_5_clay, &
      PTF_lower66_5_Db, &
      PTF_higher66_5_constant, &
      PTF_higher66_5_clay, &
      PTF_higher66_5_Db, &
      PTF_Ks_constant, &
      PTF_Ks_sand, &
      PTF_Ks_clay, &
      PTF_Ks_curveSlope, &
      rootFractionCoefficient_forest, &
      rootFractionCoefficient_impervious, &
      rootFractionCoefficient_pervious, &
      infiltrationShapeFactor, &
      jarvis_sm_threshold_c1

    if ( self%read_from_file ) then
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=soilmoisture2)
      call close_nml(unit)
      self%orgMatterContent_forest = orgMatterContent_forest
      self%orgMatterContent_impervious = orgMatterContent_impervious
      self%orgMatterContent_pervious = orgMatterContent_pervious
      self%PTF_lower66_5_constant = PTF_lower66_5_constant
      self%PTF_lower66_5_clay = PTF_lower66_5_clay
      self%PTF_lower66_5_Db = PTF_lower66_5_Db
      self%PTF_higher66_5_constant = PTF_higher66_5_constant
      self%PTF_higher66_5_clay = PTF_higher66_5_clay
      self%PTF_higher66_5_Db = PTF_higher66_5_Db
      self%PTF_Ks_constant = PTF_Ks_constant
      self%PTF_Ks_sand = PTF_Ks_sand
      self%PTF_Ks_clay = PTF_Ks_clay
      self%PTF_Ks_curveSlope = PTF_Ks_curveSlope
      self%rootFractionCoefficient_forest = rootFractionCoefficient_forest
      self%rootFractionCoefficient_impervious = rootFractionCoefficient_impervious
      self%rootFractionCoefficient_pervious = rootFractionCoefficient_pervious
      self%infiltrationShapeFactor = infiltrationShapeFactor
      self%jarvis_sm_threshold_c1 = jarvis_sm_threshold_c1
      self%read_from_file = .false.
    end if
  end subroutine read_soilmoisture2

  !> \brief Read 'soilmoisture3' namelist content.
  subroutine read_soilmoisture3(self, file, unit)
    implicit none
    class(nml_soilmoisture3_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    real(dp), dimension(nColPars) :: orgMatterContent_forest !< organic matter content [%] for forest
    real(dp), dimension(nColPars) :: orgMatterContent_impervious !< organic matter content [%] for impervious
    real(dp), dimension(nColPars) :: orgMatterContent_pervious !< organic matter content [%] for pervious
    !> Zacharias PTF parameters below 66.5 % sand content (Zacharias et al., 2007, doi:10.2136/sssaj2006.0098)
    real(dp), dimension(nColPars) :: PTF_lower66_5_constant
    real(dp), dimension(nColPars) :: PTF_lower66_5_clay !< multiplier for clay constant (see PTF_lower66_5_constant)
    real(dp), dimension(nColPars) :: PTF_lower66_5_Db !< multiplier for mineral bulk density (see PTF_lower66_5_constant)
    !> Zacharias PTF parameters above 66.5 % sand content (Zacharias et al., 2007, doi:10.2136/sssaj2006.0098)
    real(dp), dimension(nColPars) :: PTF_higher66_5_constant
    real(dp), dimension(nColPars) :: PTF_higher66_5_clay !< multiplier for clay constant (see PTF_higher66_5_constant)
    real(dp), dimension(nColPars) :: PTF_higher66_5_Db !< multiplier for mineral bulk density (see PTF_higher66_5_constant)
    !> PTF parameters for saturated hydraulic conductivity after Cosby et al. (1984)
    real(dp), dimension(nColPars) :: PTF_Ks_constant
    real(dp), dimension(nColPars) :: PTF_Ks_sand !< multiplier for sand (see PTF_Ks_constant)
    real(dp), dimension(nColPars) :: PTF_Ks_clay !< multiplier for clay (see PTF_Ks_constant)
    real(dp), dimension(nColPars) :: PTF_Ks_curveSlope !< unit conversion factor from inch/h to cm/d
    !> shape factor for root distribution with depth, which follows an exponential function [-] for forest
    real(dp), dimension(nColPars) :: rootFractionCoefficient_forest
    !> shape factor for root distribution with depth, which follows an exponential function [-] for impervious
    real(dp), dimension(nColPars) :: rootFractionCoefficient_impervious
    !> shape factor for root distribution with depth, which follows an exponential function [-] for pervious
    real(dp), dimension(nColPars) :: rootFractionCoefficient_pervious
    !> shape factor for partitioning effective precipitation into runoff and infiltration based on soil wetness [-]
    real(dp), dimension(nColPars) :: infiltrationShapeFactor
    real(dp), dimension(nColPars) :: FCmin_glob !< global field capacity minimum
    real(dp), dimension(nColPars) :: FCdelta_glob !< difference between global field capacity minimum and maximum
    real(dp), dimension(nColPars) :: rootFractionCoefficient_sand !< threshold for actual ET reduction for sand
    real(dp), dimension(nColPars) :: rootFractionCoefficient_clay !< threshold for actual ET reduction for clay
    real(dp), dimension(nColPars) :: jarvis_sm_threshold_c1 !< soil moisture threshod for jarvis model

    namelist /soilmoisture3/ &
      orgMatterContent_forest, &
      orgMatterContent_impervious, &
      orgMatterContent_pervious, &
      PTF_lower66_5_constant, &
      PTF_lower66_5_clay, &
      PTF_lower66_5_Db, &
      PTF_higher66_5_constant, &
      PTF_higher66_5_clay, &
      PTF_higher66_5_Db, &
      PTF_Ks_constant, &
      PTF_Ks_sand, &
      PTF_Ks_clay, &
      PTF_Ks_curveSlope, &
      rootFractionCoefficient_forest, &
      rootFractionCoefficient_impervious, &
      rootFractionCoefficient_pervious, &
      infiltrationShapeFactor, &
      rootFractionCoefficient_sand, &
      rootFractionCoefficient_clay, &
      FCmin_glob, &
      FCdelta_glob, &
      jarvis_sm_threshold_c1

    if ( self%read_from_file ) then
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=soilmoisture3)
      call close_nml(unit)
      self%orgMatterContent_forest = orgMatterContent_forest
      self%orgMatterContent_impervious = orgMatterContent_impervious
      self%orgMatterContent_pervious = orgMatterContent_pervious
      self%PTF_lower66_5_constant = PTF_lower66_5_constant
      self%PTF_lower66_5_clay = PTF_lower66_5_clay
      self%PTF_lower66_5_Db = PTF_lower66_5_Db
      self%PTF_higher66_5_constant = PTF_higher66_5_constant
      self%PTF_higher66_5_clay = PTF_higher66_5_clay
      self%PTF_higher66_5_Db = PTF_higher66_5_Db
      self%PTF_Ks_constant = PTF_Ks_constant
      self%PTF_Ks_sand = PTF_Ks_sand
      self%PTF_Ks_clay = PTF_Ks_clay
      self%PTF_Ks_curveSlope = PTF_Ks_curveSlope
      self%rootFractionCoefficient_forest = rootFractionCoefficient_forest
      self%rootFractionCoefficient_impervious = rootFractionCoefficient_impervious
      self%rootFractionCoefficient_pervious = rootFractionCoefficient_pervious
      self%infiltrationShapeFactor = infiltrationShapeFactor
      self%rootFractionCoefficient_sand = rootFractionCoefficient_sand
      self%rootFractionCoefficient_clay = rootFractionCoefficient_clay
      self%FCmin_glob = FCmin_glob
      self%FCdelta_glob = FCdelta_glob
      self%jarvis_sm_threshold_c1 = jarvis_sm_threshold_c1
      self%read_from_file = .false.
    end if
  end subroutine read_soilmoisture3

  !> \brief Read 'soilmoisture4' namelist content.
  subroutine read_soilmoisture4(self, file, unit)
    implicit none
    class(nml_soilmoisture4_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    real(dp), dimension(nColPars) :: orgMatterContent_forest !< organic matter content [%] for forest
    real(dp), dimension(nColPars) :: orgMatterContent_impervious !< organic matter content [%] for impervious
    real(dp), dimension(nColPars) :: orgMatterContent_pervious !< organic matter content [%] for pervious
    !> Zacharias PTF parameters below 66.5 % sand content (Zacharias et al., 2007, doi:10.2136/sssaj2006.0098)
    real(dp), dimension(nColPars) :: PTF_lower66_5_constant
    real(dp), dimension(nColPars) :: PTF_lower66_5_clay !< multiplier for clay constant (see PTF_lower66_5_constant)
    real(dp), dimension(nColPars) :: PTF_lower66_5_Db !< multiplier for mineral bulk density (see PTF_lower66_5_constant)
    !> Zacharias PTF parameters above 66.5 % sand content (Zacharias et al., 2007, doi:10.2136/sssaj2006.0098)
    real(dp), dimension(nColPars) :: PTF_higher66_5_constant
    real(dp), dimension(nColPars) :: PTF_higher66_5_clay !< multiplier for clay constant (see PTF_higher66_5_constant)
    real(dp), dimension(nColPars) :: PTF_higher66_5_Db !< multiplier for mineral bulk density (see PTF_higher66_5_constant)
    !> PTF parameters for saturated hydraulic conductivity after Cosby et al. (1984)
    real(dp), dimension(nColPars) :: PTF_Ks_constant
    real(dp), dimension(nColPars) :: PTF_Ks_sand !< multiplier for sand (see PTF_Ks_constant)
    real(dp), dimension(nColPars) :: PTF_Ks_clay !< multiplier for clay (see PTF_Ks_constant)
    real(dp), dimension(nColPars) :: PTF_Ks_curveSlope !< unit conversion factor from inch/h to cm/d
    !> shape factor for root distribution with depth, which follows an exponential function [-] for forest
    real(dp), dimension(nColPars) :: rootFractionCoefficient_forest
    !> shape factor for root distribution with depth, which follows an exponential function [-] for impervious
    real(dp), dimension(nColPars) :: rootFractionCoefficient_impervious
    !> shape factor for root distribution with depth, which follows an exponential function [-] for pervious
    real(dp), dimension(nColPars) :: rootFractionCoefficient_pervious
    !> shape factor for partitioning effective precipitation into runoff and infiltration based on soil wetness [-]
    real(dp), dimension(nColPars) :: infiltrationShapeFactor
    real(dp), dimension(nColPars) :: FCmin_glob !< global field capacity minimum
    real(dp), dimension(nColPars) :: FCdelta_glob !< difference between global field capacity minimum and maximum
    real(dp), dimension(nColPars) :: rootFractionCoefficient_sand !< threshold for actual ET reduction for sand
    real(dp), dimension(nColPars) :: rootFractionCoefficient_clay !< threshold for actual ET reduction for clay

    namelist /soilmoisture4/ &
      orgMatterContent_forest, &
      orgMatterContent_impervious, &
      orgMatterContent_pervious, &
      PTF_lower66_5_constant, &
      PTF_lower66_5_clay, &
      PTF_lower66_5_Db, &
      PTF_higher66_5_constant, &
      PTF_higher66_5_clay, &
      PTF_higher66_5_Db, &
      PTF_Ks_constant, &
      PTF_Ks_sand, &
      PTF_Ks_clay, &
      PTF_Ks_curveSlope, &
      rootFractionCoefficient_forest, &
      rootFractionCoefficient_impervious, &
      rootFractionCoefficient_pervious, &
      infiltrationShapeFactor, &
      rootFractionCoefficient_sand, &
      rootFractionCoefficient_clay, &
      FCmin_glob, &
      FCdelta_glob

    if ( self%read_from_file ) then
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=soilmoisture4)
      call close_nml(unit)
      self%orgMatterContent_forest = orgMatterContent_forest
      self%orgMatterContent_impervious = orgMatterContent_impervious
      self%orgMatterContent_pervious = orgMatterContent_pervious
      self%PTF_lower66_5_constant = PTF_lower66_5_constant
      self%PTF_lower66_5_clay = PTF_lower66_5_clay
      self%PTF_lower66_5_Db = PTF_lower66_5_Db
      self%PTF_higher66_5_constant = PTF_higher66_5_constant
      self%PTF_higher66_5_clay = PTF_higher66_5_clay
      self%PTF_higher66_5_Db = PTF_higher66_5_Db
      self%PTF_Ks_constant = PTF_Ks_constant
      self%PTF_Ks_sand = PTF_Ks_sand
      self%PTF_Ks_clay = PTF_Ks_clay
      self%PTF_Ks_curveSlope = PTF_Ks_curveSlope
      self%rootFractionCoefficient_forest = rootFractionCoefficient_forest
      self%rootFractionCoefficient_impervious = rootFractionCoefficient_impervious
      self%rootFractionCoefficient_pervious = rootFractionCoefficient_pervious
      self%infiltrationShapeFactor = infiltrationShapeFactor
      self%rootFractionCoefficient_sand = rootFractionCoefficient_sand
      self%rootFractionCoefficient_clay = rootFractionCoefficient_clay
      self%FCmin_glob = FCmin_glob
      self%FCdelta_glob = FCdelta_glob
      self%read_from_file = .false.
    end if
  end subroutine read_soilmoisture4

  !> \brief Read 'directrunoff1' namelist content.
  subroutine read_directrunoff1(self, file, unit)
    implicit none
    class(nml_directrunoff1_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    real(dp), dimension(nColPars) :: imperviousStorageCapacity !< direct Runoff: Sealed Area storage capacity

    namelist /directrunoff1/ &
      imperviousStorageCapacity

    if ( self%read_from_file ) then
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=directrunoff1)
      call close_nml(unit)
      self%imperviousStorageCapacity = imperviousStorageCapacity
      self%read_from_file = .false.
    end if
  end subroutine read_directrunoff1

  !> \brief Read 'petminus1' namelist content.
  subroutine read_petminus1(self, file, unit)
    implicit none
    class(nml_petminus1_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    real(dp), dimension(nColPars) :: PET_a_forest !< DSF=PET_a+PET_b*(1-exp(PET_c*LAI)) to correct PET as PET=DSF*PET
    real(dp), dimension(nColPars) :: PET_a_impervious !< DSF=PET_a+PET_b*(1-exp(PET_c*LAI)) to correct PET as PET=DSF*PET
    real(dp), dimension(nColPars) :: PET_a_pervious !< DSF=PET_a+PET_b*(1-exp(PET_c*LAI)) to correct PET as PET=DSF*PET
    real(dp), dimension(nColPars) :: PET_b !< DSF=PET_a+PET_b*(1-exp(PET_c*LAI)) to correct PET as PET=DSF*PET
    real(dp), dimension(nColPars) :: PET_c !< DSF=PET_a+PET_b*(1-exp(PET_c*LAI)) to correct PET as PET=DSF*PET

    namelist /petminus1/ &
      PET_a_forest, &
      PET_a_impervious, &
      PET_a_pervious, &
      PET_b, &
      PET_c

    if ( self%read_from_file ) then
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=petminus1)
      call close_nml(unit)
      self%PET_a_forest = PET_a_forest
      self%PET_a_impervious = PET_a_impervious
      self%PET_a_pervious = PET_a_pervious
      self%PET_b = PET_b
      self%PET_c = PET_c
      self%read_from_file = .false.
    end if
  end subroutine read_petminus1

  !> \brief Read 'pet0' namelist content.
  subroutine read_pet0(self, file, unit)
    implicit none
    class(nml_pet0_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    real(dp), dimension(nColPars) :: minCorrectionFactorPET !< minimum factor for PET correction with aspect
    real(dp), dimension(nColPars) :: maxCorrectionFactorPET !< maximum factor for PET correction with aspect
    real(dp), dimension(nColPars) :: aspectTresholdPET !< aspect threshold for PET correction with aspect

    namelist /pet0/ &
      minCorrectionFactorPET, &
      maxCorrectionFactorPET, &
      aspectTresholdPET

    if ( self%read_from_file ) then
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=pet0)
      call close_nml(unit)
      self%minCorrectionFactorPET = minCorrectionFactorPET
      self%maxCorrectionFactorPET = maxCorrectionFactorPET
      self%aspectTresholdPET = aspectTresholdPET
      self%read_from_file = .false.
    end if
  end subroutine read_pet0

  !> \brief Read 'pet1' namelist content.
  subroutine read_pet1(self, file, unit)
    implicit none
    class(nml_pet1_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    real(dp), dimension(nColPars) :: minCorrectionFactorPET !< minimum factor for PET correction with aspect
    real(dp), dimension(nColPars) :: maxCorrectionFactorPET !< maximum factor for PET correction with aspect
    real(dp), dimension(nColPars) :: aspectTresholdPET !< aspect threshold for PET correction with aspect
    real(dp), dimension(nColPars) :: HargreavesSamaniCoeff !< coefficient for Hargreaves Samani

    namelist /pet1/ &
    minCorrectionFactorPET, &
    maxCorrectionFactorPET, &
    aspectTresholdPET, &
    HargreavesSamaniCoeff

    if ( self%read_from_file ) then
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=pet1)
      call close_nml(unit)
      self%minCorrectionFactorPET = minCorrectionFactorPET
      self%maxCorrectionFactorPET = maxCorrectionFactorPET
      self%aspectTresholdPET = aspectTresholdPET
      self%HargreavesSamaniCoeff = HargreavesSamaniCoeff
      self%read_from_file = .false.
    end if
  end subroutine read_pet1

  !> \brief Read 'pet2' namelist content.
  subroutine read_pet2(self, file, unit)
    implicit none
    class(nml_pet2_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    real(dp), dimension(nColPars) :: PriestleyTaylorCoeff !< Priestley-Taylor coefficient
    real(dp), dimension(nColPars) :: PriestleyTaylorLAIcorr !< Priestley-Taylor LAI correction factor

    namelist /pet2/ &
      PriestleyTaylorCoeff, &
      PriestleyTaylorLAIcorr

    if ( self%read_from_file ) then
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=pet2)
      call close_nml(unit)
      self%PriestleyTaylorCoeff = PriestleyTaylorCoeff
      self%PriestleyTaylorLAIcorr = PriestleyTaylorLAIcorr
      self%read_from_file = .false.
    end if
  end subroutine read_pet2

  !> \brief Read 'pet3' namelist content.
  subroutine read_pet3(self, file, unit)
    implicit none
    class(nml_pet3_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    real(dp), dimension(nColPars) :: canopyheigth_forest !< canopy height for foreset
    real(dp), dimension(nColPars) :: canopyheigth_impervious !< canopy height for impervious
    real(dp), dimension(nColPars) :: canopyheigth_pervious !< canopy height for pervious
    real(dp), dimension(nColPars) :: displacementheight_coeff !< displacement height coefficient
    real(dp), dimension(nColPars) :: roughnesslength_momentum_coeff !< roughness length momentum coefficient
    real(dp), dimension(nColPars) :: roughnesslength_heat_coeff !< roughness length heat coefficient
    real(dp), dimension(nColPars) :: stomatal_resistance !< stomatal resistance

    namelist /pet3/ &
      canopyheigth_forest, &
      canopyheigth_impervious, &
      canopyheigth_pervious, &
      displacementheight_coeff, &
      roughnesslength_momentum_coeff, &
      roughnesslength_heat_coeff, &
      stomatal_resistance

    if ( self%read_from_file ) then
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=pet3)
      call close_nml(unit)
      self%canopyheigth_forest = canopyheigth_forest
      self%canopyheigth_impervious = canopyheigth_impervious
      self%canopyheigth_pervious = canopyheigth_pervious
      self%displacementheight_coeff = displacementheight_coeff
      self%roughnesslength_momentum_coeff = roughnesslength_momentum_coeff
      self%roughnesslength_heat_coeff = roughnesslength_heat_coeff
      self%stomatal_resistance = stomatal_resistance
      self%read_from_file = .false.
    end if
  end subroutine read_pet3

  !> \brief Read 'interflow1' namelist content.
  subroutine read_interflow1(self, file, unit)
    implicit none
    class(nml_interflow1_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    real(dp), dimension(nColPars) :: interflowStorageCapacityFactor !< interflow storage capacity factor
    real(dp), dimension(nColPars) :: interflowRecession_slope !< multiplier for slope to derive interflow recession constant
    !> multiplier to derive fast interflow recession constant for forest
    real(dp), dimension(nColPars) :: fastInterflowRecession_forest
    !> multiplier for variability of saturated hydraulic conductivity to derive slow interflow recession constant
    real(dp), dimension(nColPars) :: slowInterflowRecession_Ks
    !> multiplier for variability of saturated hydraulic conductivity to derive slow interflow exponent
    real(dp), dimension(nColPars) :: exponentSlowInterflow

    namelist /interflow1/ &
      interflowStorageCapacityFactor, &
      interflowRecession_slope, &
      fastInterflowRecession_forest, &
      slowInterflowRecession_Ks, &
      exponentSlowInterflow

    if ( self%read_from_file ) then
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=interflow1)
      call close_nml(unit)
      self%interflowStorageCapacityFactor = interflowStorageCapacityFactor
      self%interflowRecession_slope = interflowRecession_slope
      self%fastInterflowRecession_forest = fastInterflowRecession_forest
      self%slowInterflowRecession_Ks = slowInterflowRecession_Ks
      self%exponentSlowInterflow = exponentSlowInterflow
      self%read_from_file = .false.
    end if
  end subroutine read_interflow1

  !> \brief Read 'percolation1' namelist content.
  subroutine read_percolation1(self, file, unit)
    implicit none
    class(nml_percolation1_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    real(dp), dimension(nColPars) :: rechargeCoefficient !< recharge coefficient
    real(dp), dimension(nColPars) :: rechargeFactor_karstic !< recharge factor for karstic percolation
    real(dp), dimension(nColPars) :: gain_loss_GWreservoir_karstic !< gain loss in ground water reservoir for karstic

    namelist /percolation1/ &
      rechargeCoefficient, &
      rechargeFactor_karstic, &
      gain_loss_GWreservoir_karstic

    if ( self%read_from_file ) then
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=percolation1)
      call close_nml(unit)
      self%rechargeCoefficient = rechargeCoefficient
      self%rechargeFactor_karstic = rechargeFactor_karstic
      self%gain_loss_GWreservoir_karstic = gain_loss_GWreservoir_karstic
      self%read_from_file = .false.
    end if
  end subroutine read_percolation1

  !> \brief Read 'neutrons1' namelist content.
  subroutine read_neutrons1(self, file, unit)
    implicit none
    class(nml_neutrons1_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    real(dp), dimension(nColPars) :: Desilets_N0 !< Desilets N0 parameter
    real(dp), dimension(nColPars) :: Desilets_LW0 !< Desilets LW0 parameter
    real(dp), dimension(nColPars) :: Desilets_LW1 !< Desilets LW1 parameter

    namelist /neutrons1/ &
      Desilets_N0, &
      Desilets_LW0, &
      Desilets_LW1

    if ( self%read_from_file ) then
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=neutrons1)
      call close_nml(unit)
      self%Desilets_N0 = Desilets_N0
      self%Desilets_LW0 = Desilets_LW0
      self%Desilets_LW1 = Desilets_LW1
      self%read_from_file = .false.
    end if
  end subroutine read_neutrons1

  !> \brief Read 'neutrons2' namelist content.
  subroutine read_neutrons2(self, file, unit)
    implicit none
    class(nml_neutrons2_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    real(dp), dimension(nColPars) :: COSMIC_N0 !< cosmic N0 parameter
    real(dp), dimension(nColPars) :: COSMIC_N1 !< cosmic N1 parameter
    real(dp), dimension(nColPars) :: COSMIC_N2 !< cosmic N2 parameter
    real(dp), dimension(nColPars) :: COSMIC_alpha0 !< cosmic alpha0 parameter
    real(dp), dimension(nColPars) :: COSMIC_alpha1 !< cosmic alpha1 parameter
    real(dp), dimension(nColPars) :: COSMIC_L30 !< cosmic L30 parameter
    real(dp), dimension(nColPars) :: COSMIC_L31 !< cosmic L31 parameter
    real(dp), dimension(nColPars) :: COSMIC_LW0 !< cosmic LW0 parameter
    real(dp), dimension(nColPars) :: COSMIC_LW1 !< cosmic LW1 parameter

    namelist /neutrons2/ &
      COSMIC_N0, &
      COSMIC_N1, &
      COSMIC_N2, &
      COSMIC_alpha0, &
      COSMIC_alpha1, &
      COSMIC_L30, &
      COSMIC_L31, &
      COSMIC_LW0, &
      COSMIC_LW1

    if ( self%read_from_file ) then
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=neutrons2)
      call close_nml(unit)
      self%COSMIC_N0 = COSMIC_N0
      self%COSMIC_N1 = COSMIC_N1
      self%COSMIC_N2 = COSMIC_N2
      self%COSMIC_alpha0 = COSMIC_alpha0
      self%COSMIC_alpha1 = COSMIC_alpha1
      self%COSMIC_L30 = COSMIC_L30
      self%COSMIC_L31 = COSMIC_L31
      self%COSMIC_LW0 = COSMIC_LW0
      self%COSMIC_LW1 = COSMIC_LW1
      self%read_from_file = .false.
    end if
  end subroutine read_neutrons2

  !> \brief Read 'geoparameter' namelist content.
  subroutine read_geoparameter(self, file, unit)
    implicit none
    class(nml_geoparameter_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelist
    integer, intent(in) :: unit !< file unit to open the given file

    !> geological parameters (ordering according to file 'geology_classdefinition.txt')
    real(dp), dimension(maxGeoUnit, nColPars) :: GeoParam

    namelist /geoparameter/ &
      GeoParam

    if ( self%read_from_file ) then
      call open_nml(file, unit, quiet=.true.)
      call position_nml(self%name, unit)
      read(unit, nml=geoparameter)
      call close_nml(unit)
      self%GeoParam = GeoParam
      self%read_from_file = .false.
    end if
  end subroutine read_geoparameter

  ! !> \brief Read 'directories_general' namelist content.
  ! subroutine read_directories_general(self, file, unit)
  !   implicit none
  !   class(nml_directories_general_t), intent(inout) :: self
  !   character(*), intent(in) :: file !< file containing the namelist
  !   integer, intent(in) :: unit !< file unit to open the given file

  !   namelist /directories_general/ &

  !   if ( self%read_from_file ) then
  !     call open_nml(file, unit, quiet=.true.)
  !     call position_nml(self%name, unit)
  !     read(unit, nml=directories_general)
  !     call close_nml(unit)
  !     self%read_from_file = .false.
  !   end if
  ! end subroutine read_directories_general

end module mo_namelists
