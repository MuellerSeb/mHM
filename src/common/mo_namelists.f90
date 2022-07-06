!> \file    mo_namelists.f90
!> \copydoc mo_namelists

!> \brief   Module containing all namelists representations.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Jul 2022
module mo_namelists

  use mo_kind, only : i4, dp
  use mo_nml, only : open_nml, close_nml, position_nml
  use mo_common_constants, only : maxNLcovers, maxNoDomains
  use mo_common_variables, only : nProcesses
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
  ! namelist /time_periods/ &
  !   warming_Days, &
  !   eval_Per

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
  ! namelist /optional_data/ &
  !   dir_soil_moisture, &
  !   nSoilHorizons_sm_input, &
  !   dir_neutrons, &
  !   dir_evapotranspiration, &
  !   dir_TWS, &
  !   timeStep_sm_input, &
  !   timeStep_neutrons_input, &
  !   timeStep_et_input, &
  !   timeStep_tws_input
  ! namelist /panEvapo/ &
  !   evap_coeff
  ! namelist /nightDayRatio/ &
  !   read_meteo_weights, &
  !   fnight_prec, &
  !   fnight_pet, &
  !   fnight_temp, &
  !   fnight_ssrd, &
  !   fnight_strd
  ! namelist /NLoutputResults/ &
  !   output_deflate_level, &
  !   output_double_precision, &
  !   timeStep_model_outputs, &
  !   outputFlxState
  ! namelist /BFI_inputs/ &
  !   BFI_calc, &
  !   BFI_obs

  !######## mo_mpr_read_config
  ! namelist /directories_MPR/ &
  !   dir_gridded_LAI
  ! namelist /soildata/ &
  !   iFlag_soilDB, &
  !   tillageDepth, &
  !   nSoilHorizons_mHM, &
  !   soil_Depth
  ! namelist /LAI_data_information/ &
  !   inputFormat_gridded_LAI, &
  !   timeStep_LAI_input
  ! namelist /LCover_MPR/ &
  !   fracSealed_cityArea
  ! namelist /interception1/ &
  !   canopyInterceptionFactor
  ! namelist /snow1/ &
  !   snowTreshholdTemperature, &
  !   degreeDayFactor_forest, &
  !   degreeDayFactor_impervious, &
  !   degreeDayFactor_pervious, &
  !   increaseDegreeDayFactorByPrecip, &
  !   maxDegreeDayFactor_forest, &
  !   maxDegreeDayFactor_impervious, &
  !   maxDegreeDayFactor_pervious
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
  ! namelist /directRunoff1/ &
  !   imperviousStorageCapacity
  ! namelist /PETminus1/  &
  !   PET_a_forest, &
  !   PET_a_impervious, &
  !   PET_a_pervious, &
  !   PET_b, &
  !   PET_c
  ! namelist /PET0/ &
  !   minCorrectionFactorPET, &
  !   maxCorrectionFactorPET, &
  !   aspectTresholdPET
  ! namelist /PET1/ &
  !   minCorrectionFactorPET, &
  !   maxCorrectionFactorPET, &
  !   aspectTresholdPET, &
  !   HargreavesSamaniCoeff
  ! namelist /PET2/ &
  !   PriestleyTaylorCoeff, &
  !   PriestleyTaylorLAIcorr
  ! namelist /PET3/ &
  !   canopyheigth_forest, &
  !   canopyheigth_impervious, &
  !   canopyheigth_pervious, &
  !   displacementheight_coeff, &
  !   roughnesslength_momentum_coeff, &
  !   roughnesslength_heat_coeff, &
  !   stomatal_resistance
  ! namelist /interflow1/ &
  !   interflowStorageCapacityFactor, &
  !   interflowRecession_slope, &
  !   fastInterflowRecession_forest, &
  !   slowInterflowRecession_Ks, &
  !   exponentSlowInterflow
  ! namelist /percolation1/ &
  !   rechargeCoefficient, &
  !   rechargeFactor_karstic, &
  !   gain_loss_GWreservoir_karstic
  ! namelist /neutrons1/ &
  !   Desilets_N0, &
  !   Desilets_LW0, &
  !   Desilets_LW1
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
  ! namelist /geoparameter/ &
  !   GeoParam

  !######## mo_mrm_read_config
  ! namelist /mainconfig_mrm/ &
  !   ALMA_convention, &
  !   filenameTotalRunoff, &
  !   varnameTotalRunoff, &
  !   gw_coupling
  ! namelist /directories_mRM/ &
  !   dir_Gauges, &
  !   dir_Total_Runoff, &
  !   dir_Bankfull_Runoff
  ! namelist /evaluation_gauges/ &
  !   nGaugesTotal, &
  !   NoGauges_domain, &
  !   Gauge_id, &
  !   gauge_filename
  ! namelist /inflow_gauges/ &
  !   nInflowGaugesTotal, &
  !   NoInflowGauges_domain, &
  !   InflowGauge_id, &
  !   InflowGauge_filename, &
  !   InflowGauge_Headwater
  ! namelist /NLoutputResults/ &
  !   output_deflate_level_mrm, &
  !   output_double_precision_mrm, &
  !   timeStep_model_outputs_mrm, &
  !   outputFlxState_mrm
  ! namelist /routing1/ &
  !   muskingumTravelTime_constant, &
  !   muskingumTravelTime_riverLength, &
  !   muskingumTravelTime_riverSlope, &
  !   muskingumTravelTime_impervious, &
  !   muskingumAttenuation_riverSlope
  ! namelist /routing2/ &
  !   streamflow_celerity
  ! namelist /routing3/ &
  !   slope_factor

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
      call open_nml(file, unit, quiet =.true.)
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
      call open_nml(file, unit, quiet =.true.)
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
      call open_nml(file, unit, quiet =.true.)
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
      call open_nml(file, unit, quiet =.true.)
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
      call open_nml(file, unit, quiet =.true.)
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

  ! !> \brief Read 'directories_general' namelist content.
  ! subroutine read_directories_general(self, file, unit)
  !   implicit none
  !   class(nml_directories_general_t), intent(inout) :: self
  !   character(*), intent(in) :: file !< file containing the namelist
  !   integer, intent(in) :: unit !< file unit to open the given file

  !   namelist /directories_general/ &

  !   if ( self%read_from_file ) then
  !     call open_nml(file, unit, quiet =.true.)
  !     call position_nml(self%name, unit)
  !     read(unit, nml=directories_general)
  !     call close_nml(unit)
  !     self%read_from_file = .false.
  !   end if
  ! end subroutine read_directories_general

end module mo_namelists
