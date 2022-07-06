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
  use mo_common_constants, only : maxNLcovers, maxNoDomains
  use mo_common_variables, only : nProcesses, period
  use mo_common_mHM_mRM_variables, only : nerror_model

  implicit none

  type, private :: nml_base
    logical :: read_from_file = .true. !< whether the associated variables are already set by interfaces
  end type nml_base

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
  type, extends(nml_base), public :: nml_project_description_t
    character(19) :: name = "project_description" !< namelist name
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
  type, extends(nml_base), public :: nml_directories_general_t
    character(19) :: name = "directories_general" !< namelist name
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
  type, extends(nml_base), public :: nml_mainconfig_t
    character(10) :: name = "mainconfig" !< namelist name
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
  type, extends(nml_base), public :: nml_processselection_t
    character(16) :: name = "processSelection" !< namelist name
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
  type, extends(nml_base), public :: nml_lcover_t
    character(6) :: name = "LCover" !< namelist name
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
  type, extends(nml_base), public :: nml_mainconfig_mhm_mrm_t
    character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
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
  type, extends(nml_base), public :: nml_Optimization_t
    character(12) :: name = "Optimization" !< namelist name
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
  type, extends(nml_base), public :: nml_time_periods_t
    character(12) :: name = "time_periods" !< namelist name
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
  type, extends(nml_base), public :: nml_directories_mHM_t
    character(15) :: name = "directories_mHM" !< namelist name
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
  type, extends(nml_base), public :: nml_optional_data_t
    character(13) :: name = "optional_data" !< namelist name
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
  type, extends(nml_base), public :: nml_panEvapo_t
    character(8) :: name = "panEvapo" !< namelist name
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
  type, extends(nml_base), public :: nml_nightDayRatio_t
    character(18) :: name = "nightDayRatio" !< namelist name
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
  type, extends(nml_base), public :: nml_NLoutputResults_t
    character(15) :: name = "NLoutputResults" !< namelist name
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
  type, extends(nml_base), public :: nml_BFI_inputs_t
    character(18) :: name = "BFI_inputs" !< namelist name
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
  ! !> \class   nml_mainconfig_mhm_mrm_t
  ! !> \brief   'mainconfig_mhm_mrm' namelist content
  ! type, extends(nml_base), public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
  ! contains
  !   !> \copydoc mo_namelists::read_mainconfig_mhm_mrm
  !   procedure, public :: read => read_mainconfig_mhm_mrm !< \see mo_namelists::read_mainconfig_mhm_mrm
  ! end type nml_mainconfig_mhm_mrm_t
  ! !> 'mainconfig_mhm_mrm' namelist content
  ! type(nml_mainconfig_mhm_mrm_t), public :: nml_mainconfig_mhm_mrm

  ! namelist /soildata/ &
  !   iFlag_soilDB, &
  !   tillageDepth, &
  !   nSoilHorizons_mHM, &
  !   soil_Depth
  !
  ! !> \class   nml_mainconfig_mhm_mrm_t
  ! !> \brief   'mainconfig_mhm_mrm' namelist content
  ! type, extends(nml_base), public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
  ! contains
  !   !> \copydoc mo_namelists::read_mainconfig_mhm_mrm
  !   procedure, public :: read => read_mainconfig_mhm_mrm !< \see mo_namelists::read_mainconfig_mhm_mrm
  ! end type nml_mainconfig_mhm_mrm_t
  ! !> 'mainconfig_mhm_mrm' namelist content
  ! type(nml_mainconfig_mhm_mrm_t), public :: nml_mainconfig_mhm_mrm

  ! namelist /LAI_data_information/ &
  !   inputFormat_gridded_LAI, &
  !   timeStep_LAI_input
  !
  ! !> \class   nml_mainconfig_mhm_mrm_t
  ! !> \brief   'mainconfig_mhm_mrm' namelist content
  ! type, extends(nml_base), public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
  ! contains
  !   !> \copydoc mo_namelists::read_mainconfig_mhm_mrm
  !   procedure, public :: read => read_mainconfig_mhm_mrm !< \see mo_namelists::read_mainconfig_mhm_mrm
  ! end type nml_mainconfig_mhm_mrm_t
  ! !> 'mainconfig_mhm_mrm' namelist content
  ! type(nml_mainconfig_mhm_mrm_t), public :: nml_mainconfig_mhm_mrm

  ! namelist /LCover_MPR/ &
  !   fracSealed_cityArea
  !
  ! !> \class   nml_mainconfig_mhm_mrm_t
  ! !> \brief   'mainconfig_mhm_mrm' namelist content
  ! type, extends(nml_base), public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
  ! contains
  !   !> \copydoc mo_namelists::read_mainconfig_mhm_mrm
  !   procedure, public :: read => read_mainconfig_mhm_mrm !< \see mo_namelists::read_mainconfig_mhm_mrm
  ! end type nml_mainconfig_mhm_mrm_t
  ! !> 'mainconfig_mhm_mrm' namelist content
  ! type(nml_mainconfig_mhm_mrm_t), public :: nml_mainconfig_mhm_mrm

  ! namelist /interception1/ &
  !   canopyInterceptionFactor
  !
  ! !> \class   nml_mainconfig_mhm_mrm_t
  ! !> \brief   'mainconfig_mhm_mrm' namelist content
  ! type, extends(nml_base), public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
  ! contains
  !   !> \copydoc mo_namelists::read_mainconfig_mhm_mrm
  !   procedure, public :: read => read_mainconfig_mhm_mrm !< \see mo_namelists::read_mainconfig_mhm_mrm
  ! end type nml_mainconfig_mhm_mrm_t
  ! !> 'mainconfig_mhm_mrm' namelist content
  ! type(nml_mainconfig_mhm_mrm_t), public :: nml_mainconfig_mhm_mrm

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
  ! !> \class   nml_mainconfig_mhm_mrm_t
  ! !> \brief   'mainconfig_mhm_mrm' namelist content
  ! type, extends(nml_base), public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
  ! contains
  !   !> \copydoc mo_namelists::read_mainconfig_mhm_mrm
  !   procedure, public :: read => read_mainconfig_mhm_mrm !< \see mo_namelists::read_mainconfig_mhm_mrm
  ! end type nml_mainconfig_mhm_mrm_t
  ! !> 'mainconfig_mhm_mrm' namelist content
  ! type(nml_mainconfig_mhm_mrm_t), public :: nml_mainconfig_mhm_mrm

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
  ! !> \class   nml_mainconfig_mhm_mrm_t
  ! !> \brief   'mainconfig_mhm_mrm' namelist content
  ! type, extends(nml_base), public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
  ! contains
  !   !> \copydoc mo_namelists::read_mainconfig_mhm_mrm
  !   procedure, public :: read => read_mainconfig_mhm_mrm !< \see mo_namelists::read_mainconfig_mhm_mrm
  ! end type nml_mainconfig_mhm_mrm_t
  ! !> 'mainconfig_mhm_mrm' namelist content
  ! type(nml_mainconfig_mhm_mrm_t), public :: nml_mainconfig_mhm_mrm

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
  ! !> \class   nml_mainconfig_mhm_mrm_t
  ! !> \brief   'mainconfig_mhm_mrm' namelist content
  ! type, extends(nml_base), public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
  ! contains
  !   !> \copydoc mo_namelists::read_mainconfig_mhm_mrm
  !   procedure, public :: read => read_mainconfig_mhm_mrm !< \see mo_namelists::read_mainconfig_mhm_mrm
  ! end type nml_mainconfig_mhm_mrm_t
  ! !> 'mainconfig_mhm_mrm' namelist content
  ! type(nml_mainconfig_mhm_mrm_t), public :: nml_mainconfig_mhm_mrm

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
  !   infiltrationShapeFactor,rootFractionCoefficient_sand, &
  !   rootFractionCoefficient_clay, &
  !   FCmin_glob, &
  !   FCdelta_glob, &
  !   jarvis_sm_threshold_c1
  !
  ! !> \class   nml_mainconfig_mhm_mrm_t
  ! !> \brief   'mainconfig_mhm_mrm' namelist content
  ! type, extends(nml_base), public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
  ! contains
  !   !> \copydoc mo_namelists::read_mainconfig_mhm_mrm
  !   procedure, public :: read => read_mainconfig_mhm_mrm !< \see mo_namelists::read_mainconfig_mhm_mrm
  ! end type nml_mainconfig_mhm_mrm_t
  ! !> 'mainconfig_mhm_mrm' namelist content
  ! type(nml_mainconfig_mhm_mrm_t), public :: nml_mainconfig_mhm_mrm

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
  !   infiltrationShapeFactor,rootFractionCoefficient_sand, &
  !   rootFractionCoefficient_clay, &
  !   FCmin_glob, &
  !   FCdelta_glob
  !
  ! !> \class   nml_mainconfig_mhm_mrm_t
  ! !> \brief   'mainconfig_mhm_mrm' namelist content
  ! type, extends(nml_base), public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
  ! contains
  !   !> \copydoc mo_namelists::read_mainconfig_mhm_mrm
  !   procedure, public :: read => read_mainconfig_mhm_mrm !< \see mo_namelists::read_mainconfig_mhm_mrm
  ! end type nml_mainconfig_mhm_mrm_t
  ! !> 'mainconfig_mhm_mrm' namelist content
  ! type(nml_mainconfig_mhm_mrm_t), public :: nml_mainconfig_mhm_mrm

  ! namelist /directRunoff1/ &
  !   imperviousStorageCapacity
  !
  ! !> \class   nml_mainconfig_mhm_mrm_t
  ! !> \brief   'mainconfig_mhm_mrm' namelist content
  ! type, extends(nml_base), public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
  ! contains
  !   !> \copydoc mo_namelists::read_mainconfig_mhm_mrm
  !   procedure, public :: read => read_mainconfig_mhm_mrm !< \see mo_namelists::read_mainconfig_mhm_mrm
  ! end type nml_mainconfig_mhm_mrm_t
  ! !> 'mainconfig_mhm_mrm' namelist content
  ! type(nml_mainconfig_mhm_mrm_t), public :: nml_mainconfig_mhm_mrm

  ! namelist /PETminus1/  &
  !   PET_a_forest, &
  !   PET_a_impervious, &
  !   PET_a_pervious, &
  !   PET_b, &
  !   PET_c
  !
  ! !> \class   nml_mainconfig_mhm_mrm_t
  ! !> \brief   'mainconfig_mhm_mrm' namelist content
  ! type, extends(nml_base), public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
  ! contains
  !   !> \copydoc mo_namelists::read_mainconfig_mhm_mrm
  !   procedure, public :: read => read_mainconfig_mhm_mrm !< \see mo_namelists::read_mainconfig_mhm_mrm
  ! end type nml_mainconfig_mhm_mrm_t
  ! !> 'mainconfig_mhm_mrm' namelist content
  ! type(nml_mainconfig_mhm_mrm_t), public :: nml_mainconfig_mhm_mrm

  ! namelist /PET0/ &
  !   minCorrectionFactorPET, &
  !   maxCorrectionFactorPET, &
  !   aspectTresholdPET
  !
  ! !> \class   nml_mainconfig_mhm_mrm_t
  ! !> \brief   'mainconfig_mhm_mrm' namelist content
  ! type, extends(nml_base), public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
  ! contains
  !   !> \copydoc mo_namelists::read_mainconfig_mhm_mrm
  !   procedure, public :: read => read_mainconfig_mhm_mrm !< \see mo_namelists::read_mainconfig_mhm_mrm
  ! end type nml_mainconfig_mhm_mrm_t
  ! !> 'mainconfig_mhm_mrm' namelist content
  ! type(nml_mainconfig_mhm_mrm_t), public :: nml_mainconfig_mhm_mrm

  ! namelist /PET1/ &
  !   minCorrectionFactorPET, &
  !   maxCorrectionFactorPET, &
  !   aspectTresholdPET, &
  !   HargreavesSamaniCoeff
  !
  ! !> \class   nml_mainconfig_mhm_mrm_t
  ! !> \brief   'mainconfig_mhm_mrm' namelist content
  ! type, extends(nml_base), public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
  ! contains
  !   !> \copydoc mo_namelists::read_mainconfig_mhm_mrm
  !   procedure, public :: read => read_mainconfig_mhm_mrm !< \see mo_namelists::read_mainconfig_mhm_mrm
  ! end type nml_mainconfig_mhm_mrm_t
  ! !> 'mainconfig_mhm_mrm' namelist content
  ! type(nml_mainconfig_mhm_mrm_t), public :: nml_mainconfig_mhm_mrm

  ! namelist /PET2/ &
  !   PriestleyTaylorCoeff, &
  !   PriestleyTaylorLAIcorr
  !
  ! !> \class   nml_mainconfig_mhm_mrm_t
  ! !> \brief   'mainconfig_mhm_mrm' namelist content
  ! type, extends(nml_base), public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
  ! contains
  !   !> \copydoc mo_namelists::read_mainconfig_mhm_mrm
  !   procedure, public :: read => read_mainconfig_mhm_mrm !< \see mo_namelists::read_mainconfig_mhm_mrm
  ! end type nml_mainconfig_mhm_mrm_t
  ! !> 'mainconfig_mhm_mrm' namelist content
  ! type(nml_mainconfig_mhm_mrm_t), public :: nml_mainconfig_mhm_mrm

  ! namelist /PET3/ &
  !   canopyheigth_forest, &
  !   canopyheigth_impervious, &
  !   canopyheigth_pervious, &
  !   displacementheight_coeff, &
  !   roughnesslength_momentum_coeff, &
  !   roughnesslength_heat_coeff, &
  !   stomatal_resistance
  !
  ! !> \class   nml_mainconfig_mhm_mrm_t
  ! !> \brief   'mainconfig_mhm_mrm' namelist content
  ! type, extends(nml_base), public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
  ! contains
  !   !> \copydoc mo_namelists::read_mainconfig_mhm_mrm
  !   procedure, public :: read => read_mainconfig_mhm_mrm !< \see mo_namelists::read_mainconfig_mhm_mrm
  ! end type nml_mainconfig_mhm_mrm_t
  ! !> 'mainconfig_mhm_mrm' namelist content
  ! type(nml_mainconfig_mhm_mrm_t), public :: nml_mainconfig_mhm_mrm

  ! namelist /interflow1/ &
  !   interflowStorageCapacityFactor, &
  !   interflowRecession_slope, &
  !   fastInterflowRecession_forest, &
  !   slowInterflowRecession_Ks, &
  !   exponentSlowInterflow
  !
  ! !> \class   nml_mainconfig_mhm_mrm_t
  ! !> \brief   'mainconfig_mhm_mrm' namelist content
  ! type, extends(nml_base), public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
  ! contains
  !   !> \copydoc mo_namelists::read_mainconfig_mhm_mrm
  !   procedure, public :: read => read_mainconfig_mhm_mrm !< \see mo_namelists::read_mainconfig_mhm_mrm
  ! end type nml_mainconfig_mhm_mrm_t
  ! !> 'mainconfig_mhm_mrm' namelist content
  ! type(nml_mainconfig_mhm_mrm_t), public :: nml_mainconfig_mhm_mrm

  ! namelist /percolation1/ &
  !   rechargeCoefficient, &
  !   rechargeFactor_karstic, &
  !   gain_loss_GWreservoir_karstic
  !
  ! !> \class   nml_mainconfig_mhm_mrm_t
  ! !> \brief   'mainconfig_mhm_mrm' namelist content
  ! type, extends(nml_base), public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
  ! contains
  !   !> \copydoc mo_namelists::read_mainconfig_mhm_mrm
  !   procedure, public :: read => read_mainconfig_mhm_mrm !< \see mo_namelists::read_mainconfig_mhm_mrm
  ! end type nml_mainconfig_mhm_mrm_t
  ! !> 'mainconfig_mhm_mrm' namelist content
  ! type(nml_mainconfig_mhm_mrm_t), public :: nml_mainconfig_mhm_mrm

  ! namelist /neutrons1/ &
  !   Desilets_N0, &
  !   Desilets_LW0, &
  !   Desilets_LW1
  !
  ! !> \class   nml_mainconfig_mhm_mrm_t
  ! !> \brief   'mainconfig_mhm_mrm' namelist content
  ! type, extends(nml_base), public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
  ! contains
  !   !> \copydoc mo_namelists::read_mainconfig_mhm_mrm
  !   procedure, public :: read => read_mainconfig_mhm_mrm !< \see mo_namelists::read_mainconfig_mhm_mrm
  ! end type nml_mainconfig_mhm_mrm_t
  ! !> 'mainconfig_mhm_mrm' namelist content
  ! type(nml_mainconfig_mhm_mrm_t), public :: nml_mainconfig_mhm_mrm

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
  ! !> \class   nml_mainconfig_mhm_mrm_t
  ! !> \brief   'mainconfig_mhm_mrm' namelist content
  ! type, extends(nml_base), public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
  ! contains
  !   !> \copydoc mo_namelists::read_mainconfig_mhm_mrm
  !   procedure, public :: read => read_mainconfig_mhm_mrm !< \see mo_namelists::read_mainconfig_mhm_mrm
  ! end type nml_mainconfig_mhm_mrm_t
  ! !> 'mainconfig_mhm_mrm' namelist content
  ! type(nml_mainconfig_mhm_mrm_t), public :: nml_mainconfig_mhm_mrm

  ! namelist /geoparameter/ &
  !   GeoParam
  !
  ! !> \class   nml_mainconfig_mhm_mrm_t
  ! !> \brief   'mainconfig_mhm_mrm' namelist content
  ! type, extends(nml_base), public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
  ! contains
  !   !> \copydoc mo_namelists::read_mainconfig_mhm_mrm
  !   procedure, public :: read => read_mainconfig_mhm_mrm !< \see mo_namelists::read_mainconfig_mhm_mrm
  ! end type nml_mainconfig_mhm_mrm_t
  ! !> 'mainconfig_mhm_mrm' namelist content
  ! type(nml_mainconfig_mhm_mrm_t), public :: nml_mainconfig_mhm_mrm

  !######## mo_mrm_read_config
  ! namelist /mainconfig_mrm/ &
  !   ALMA_convention, &
  !   filenameTotalRunoff, &
  !   varnameTotalRunoff, &
  !   gw_coupling
  !
  ! !> \class   nml_mainconfig_mhm_mrm_t
  ! !> \brief   'mainconfig_mhm_mrm' namelist content
  ! type, extends(nml_base), public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
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
  ! type, extends(nml_base), public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
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
  ! type, extends(nml_base), public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
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
  ! type, extends(nml_base), public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
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
  ! type, extends(nml_base), public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
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
  ! type, extends(nml_base), public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
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
  ! type, extends(nml_base), public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
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
  ! type, extends(nml_base), public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
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
  ! type, extends(nml_base), public :: nml_mainconfig_mhm_mrm_t
  !   character(18) :: name = "mainconfig_mhm_mrm" !< namelist name
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
