!> \file    mo_main_config.f90
!> \brief   \copybrief mo_main_config
!> \details \copydetails mo_main_config

!> \brief   Module for a mHM process container.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Aug 2025
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_exchange
module mo_main_config
  use mo_kind, only: i4, dp
  use mo_common_constants, only: nprocesses
  use mo_message, only: message, error_message
  use mo_string_utils, only: n2s => num2str
  use mo_namelists, only: &
    ! mhm.nml
    nml_mainconfig_t, &
    nml_processselection_t, &
    ! parameters.nml
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

  !> \class   main_config_t
  !> \brief   Main configuration.
  type, public :: main_config_t
    character(:), allocatable :: file    !< mhm namelist file
    type(nml_mainconfig_t) :: mainconfig !< mainconfig configuration
  contains
    procedure :: read => main_config_read
  end type main_config_t

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

  !> \class   parameters_t
  !> \brief   Class for parameters and processes configuration.
  type, public :: parameters_t
    type(parameter_config_t) :: config !< configuration of the mRM process container
    type(process_config_t) :: process_config !< configuration of the mRM process container
    !> Info about which process runs in which option and number of parameters necessary for this option
    !! - col1: process switch
    !! - col2: number of parameters for process case
    !! - col3: cumulated number of parameters in the array so far
    integer(i4), dimension(nprocesses, 3) :: process_matrix
    !> Matrix of global parameters definition (former: gamma)
    !! - col1: min
    !! - col2: max
    !! - col3: initial
    !! - col4: flag
    !! - col5: scaling
    real(dp), dimension(:, :), allocatable :: definition
    !> Array of global parameters names
    character(256), dimension(:), allocatable :: names
    !> Array of current parameter set
    real(dp), dimension(:), allocatable :: values
    integer(i4) :: nGeoUnits !< Number of geological formations
  contains
    procedure :: init => parameters_init
    procedure :: create => parameters_create
    procedure :: set => parameters_set
    procedure :: get => parameters_get
    procedure :: get_process => parameters_get_process
    procedure :: print => parameters_print
  end type parameters_t

contains

  !> \brief Read main configuration.
  subroutine main_config_read(self, file)
    class(main_config_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelists
    call message(" ... config main: ", file)
    self%file = file
    call self%mainconfig%read(file)
  end subroutine main_config_read

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

  !> \brief Initialize the mRM process container.
  subroutine parameters_init(self, parameter_cfg, process_cfg)
    class(parameters_t), intent(inout) :: self
    type(parameter_config_t), intent(in) :: parameter_cfg !< parameters configuration
    type(process_config_t), intent(in) :: process_cfg !< process configuration
    call message(" ... init parameters")
    self%config = parameter_cfg
    self%process_config = process_cfg
    call self%create()
  end subroutine parameters_init

  !>       \brief Configure parameters
  !>       \authors Stephan Thober
  !>       \date Aug 2015
  ! Modifications:
  ! Stephan Thober  Sep 2015 - removed stop condition when routing resolution is smaller than hydrologic resolution
  ! Stephan Thober  Oct 2015 - added NLoutputResults namelist, fileLatLon to directories_general namelist, and readLatLon flag
  ! Robert Schweppe Dec 2017 - adapted for MPR
  !  Rohini Kumar   Oct 2021 - Added Neutron count module to mHM integrate into develop branch (5.11.2)
  subroutine parameters_create(self)

    use mo_append, only : append
    use mo_common_constants, only : maxNoDomains, nColPars
    use mo_mpr_constants, only : maxGeoUnit
    use mo_constants, only : eps_dp, nodata_dp
    use mo_common_functions, only : in_bound
    use mo_message, only : message, error_message
    use mo_string_utils, only : num2str
    use mo_utils, only : EQ

    implicit none
    class(parameters_t), intent(inout) :: self

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
        if (self%config%interception1%nml_status /= 0_i4) call error_message("'interception1' namelist not found.")

        self%process_matrix(1, 2) = 1_i4
        self%process_matrix(1, 3) = 1_i4
        call append(self%definition, reshape(self%config%interception1%canopyInterceptionFactor, shp))

        call append(self%names, [  &
                'canopyInterceptionFactor'])

        ! check if parameter are in range
        if (.not. in_bound(self%definition)) &
          call error_message('***ERROR: parameter in namelist "interception1" out of bound in ', self%config%file)

      case DEFAULT
        call error_message('***ERROR: Process description for process "interception" does not exist!')
        stop
    end select

    ! Process 2 - snow
    select case (self%process_matrix(2, 1))
      ! 1 - degree-day approach
      case(1)
        if (self%config%snow1%nml_status /= 0_i4) call error_message("'snow1' namelist not found.")

        self%process_matrix(2, 2) = 8_i4
        self%process_matrix(2, 3) = sum(self%process_matrix(1 : 2, 2))
        call append(self%definition, reshape(self%config%snow1%snowTreshholdTemperature, shp))
        call append(self%definition, reshape(self%config%snow1%degreeDayFactor_forest, shp))
        call append(self%definition, reshape(self%config%snow1%degreeDayFactor_impervious, shp))
        call append(self%definition, reshape(self%config%snow1%degreeDayFactor_pervious, shp))
        call append(self%definition, reshape(self%config%snow1%increaseDegreeDayFactorByPrecip, shp))
        call append(self%definition, reshape(self%config%snow1%maxDegreeDayFactor_forest, shp))
        call append(self%definition, reshape(self%config%snow1%maxDegreeDayFactor_impervious, shp))
        call append(self%definition, reshape(self%config%snow1%maxDegreeDayFactor_pervious, shp))

        call append(self%names, [  &
                'snowTreshholdTemperature       ', &
                'degreeDayFactor_forest         ', &
                'degreeDayFactor_impervious     ', &
                'degreeDayFactor_pervious       ', &
                'increaseDegreeDayFactorByPrecip', &
                'maxDegreeDayFactor_forest      ', &
                'maxDegreeDayFactor_impervious  ', &
                'maxDegreeDayFactor_pervious    '])

        ! check if parameter are in range
        if (.not. in_bound(self%definition)) &
          call error_message('***ERROR: parameter in namelist "snow1" out of bound in ', self%config%file)

      case DEFAULT
        call error_message('***ERROR: Process description for process "snow" does not exist!')
    end select

    ! Process 3 - soilmoisture
    select case (self%process_matrix(3, 1))

        ! 1 - Feddes equation for PET reduction, bucket approach, Brooks-Corey like
      case(1)
        if (self%config%soilmoisture1%nml_status /= 0_i4) call error_message("'soilmoisture1' namelist not found.")

        self%process_matrix(3, 2) = 17_i4
        self%process_matrix(3, 3) = sum(self%process_matrix(1 : 3, 2))
        call append(self%definition, reshape(self%config%soilmoisture1%orgMatterContent_forest, shp))
        call append(self%definition, reshape(self%config%soilmoisture1%orgMatterContent_impervious, shp))
        call append(self%definition, reshape(self%config%soilmoisture1%orgMatterContent_pervious, shp))
        call append(self%definition, reshape(self%config%soilmoisture1%PTF_lower66_5_constant, shp))
        call append(self%definition, reshape(self%config%soilmoisture1%PTF_lower66_5_clay, shp))
        call append(self%definition, reshape(self%config%soilmoisture1%PTF_lower66_5_Db, shp))
        call append(self%definition, reshape(self%config%soilmoisture1%PTF_higher66_5_constant, shp))
        call append(self%definition, reshape(self%config%soilmoisture1%PTF_higher66_5_clay, shp))
        call append(self%definition, reshape(self%config%soilmoisture1%PTF_higher66_5_Db, shp))
        call append(self%definition, reshape(self%config%soilmoisture1%PTF_Ks_constant, shp))
        call append(self%definition, reshape(self%config%soilmoisture1%PTF_Ks_sand, shp))
        call append(self%definition, reshape(self%config%soilmoisture1%PTF_Ks_clay, shp))
        call append(self%definition, reshape(self%config%soilmoisture1%PTF_Ks_curveSlope, shp))
        call append(self%definition, reshape(self%config%soilmoisture1%rootFractionCoefficient_forest, shp))
        call append(self%definition, reshape(self%config%soilmoisture1%rootFractionCoefficient_impervious, shp))
        call append(self%definition, reshape(self%config%soilmoisture1%rootFractionCoefficient_pervious, shp))
        call append(self%definition, reshape(self%config%soilmoisture1%infiltrationShapeFactor, shp))

        call append(self%names, [     &
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
        if (.not. in_bound(self%definition)) &
          call error_message('***ERROR: parameter in namelist "soilmoisture1" out of bound in ', self%config%file)

        ! 2- Jarvis equation for PET reduction, bucket approach, Brooks-Corey like
      case(2)
        if (self%config%soilmoisture2%nml_status /= 0_i4) call error_message("'soilmoisture2' namelist not found.")

        self%process_matrix(3, 2) = 18_i4
        self%process_matrix(3, 3) = sum(self%process_matrix(1 : 3, 2))
        call append(self%definition, reshape(self%config%soilmoisture2%orgMatterContent_forest, shp))
        call append(self%definition, reshape(self%config%soilmoisture2%orgMatterContent_impervious, shp))
        call append(self%definition, reshape(self%config%soilmoisture2%orgMatterContent_pervious, shp))
        call append(self%definition, reshape(self%config%soilmoisture2%PTF_lower66_5_constant, shp))
        call append(self%definition, reshape(self%config%soilmoisture2%PTF_lower66_5_clay, shp))
        call append(self%definition, reshape(self%config%soilmoisture2%PTF_lower66_5_Db, shp))
        call append(self%definition, reshape(self%config%soilmoisture2%PTF_higher66_5_constant, shp))
        call append(self%definition, reshape(self%config%soilmoisture2%PTF_higher66_5_clay, shp))
        call append(self%definition, reshape(self%config%soilmoisture2%PTF_higher66_5_Db, shp))
        call append(self%definition, reshape(self%config%soilmoisture2%PTF_Ks_constant, shp))
        call append(self%definition, reshape(self%config%soilmoisture2%PTF_Ks_sand, shp))
        call append(self%definition, reshape(self%config%soilmoisture2%PTF_Ks_clay, shp))
        call append(self%definition, reshape(self%config%soilmoisture2%PTF_Ks_curveSlope, shp))
        call append(self%definition, reshape(self%config%soilmoisture2%rootFractionCoefficient_forest, shp))
        call append(self%definition, reshape(self%config%soilmoisture2%rootFractionCoefficient_impervious, shp))
        call append(self%definition, reshape(self%config%soilmoisture2%rootFractionCoefficient_pervious, shp))
        call append(self%definition, reshape(self%config%soilmoisture2%infiltrationShapeFactor, shp))
        call append(self%definition, reshape(self%config%soilmoisture2%jarvis_sm_threshold_c1, shp))

        call append(self%names, [     &
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
        if (.not. in_bound(self%definition)) &
          call error_message('***ERROR: parameter in namelist "soilmoisture2" out of bound in ', self%config%file)

        ! 3- Jarvis equation for ET reduction and FC dependency on root fraction coefficient
      case(3)
        if (self%config%soilmoisture3%nml_status /= 0_i4) call error_message("'soilmoisture3' namelist not found.")

        self%process_matrix(3, 2) = 22_i4
        self%process_matrix(3, 3) = sum(self%process_matrix(1 : 3, 2))
        call append(self%definition, reshape(self%config%soilmoisture3%orgMatterContent_forest, shp))
        call append(self%definition, reshape(self%config%soilmoisture3%orgMatterContent_impervious, shp))
        call append(self%definition, reshape(self%config%soilmoisture3%orgMatterContent_pervious, shp))
        call append(self%definition, reshape(self%config%soilmoisture3%PTF_lower66_5_constant, shp))
        call append(self%definition, reshape(self%config%soilmoisture3%PTF_lower66_5_clay, shp))
        call append(self%definition, reshape(self%config%soilmoisture3%PTF_lower66_5_Db, shp))
        call append(self%definition, reshape(self%config%soilmoisture3%PTF_higher66_5_constant, shp))
        call append(self%definition, reshape(self%config%soilmoisture3%PTF_higher66_5_clay, shp))
        call append(self%definition, reshape(self%config%soilmoisture3%PTF_higher66_5_Db, shp))
        call append(self%definition, reshape(self%config%soilmoisture3%PTF_Ks_constant, shp))
        call append(self%definition, reshape(self%config%soilmoisture3%PTF_Ks_sand, shp))
        call append(self%definition, reshape(self%config%soilmoisture3%PTF_Ks_clay, shp))
        call append(self%definition, reshape(self%config%soilmoisture3%PTF_Ks_curveSlope, shp))
        call append(self%definition, reshape(self%config%soilmoisture3%rootFractionCoefficient_forest, shp))
        call append(self%definition, reshape(self%config%soilmoisture3%rootFractionCoefficient_impervious, shp))
        call append(self%definition, reshape(self%config%soilmoisture3%rootFractionCoefficient_pervious, shp))
        call append(self%definition, reshape(self%config%soilmoisture3%infiltrationShapeFactor, shp))
        call append(self%definition, reshape(self%config%soilmoisture3%rootFractionCoefficient_sand, shp))
        call append(self%definition, reshape(self%config%soilmoisture3%rootFractionCoefficient_clay, shp))
        call append(self%definition, reshape(self%config%soilmoisture3%FCmin_glob, shp))
        call append(self%definition, reshape(self%config%soilmoisture3%FCdelta_glob, shp))
        call append(self%definition, reshape(self%config%soilmoisture3%jarvis_sm_threshold_c1, shp))

        call append(self%names, [     &
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
        if (.not. in_bound(self%definition)) &
          call error_message('***ERROR: parameter in namelist "soilmoisture3" out of bound in ', self%config%file)

        ! 4- Feddes equation for ET reduction and FC dependency on root fraction coefficient
      case(4)
        if (self%config%soilmoisture4%nml_status /= 0_i4) call error_message("'soilmoisture4' namelist not found.")

        self%process_matrix(3, 2) = 21_i4
        self%process_matrix(3, 3) = sum(self%process_matrix(1 : 3, 2))
        call append(self%definition, reshape(self%config%soilmoisture4%orgMatterContent_forest, shp))
        call append(self%definition, reshape(self%config%soilmoisture4%orgMatterContent_impervious, shp))
        call append(self%definition, reshape(self%config%soilmoisture4%orgMatterContent_pervious, shp))
        call append(self%definition, reshape(self%config%soilmoisture4%PTF_lower66_5_constant, shp))
        call append(self%definition, reshape(self%config%soilmoisture4%PTF_lower66_5_clay, shp))
        call append(self%definition, reshape(self%config%soilmoisture4%PTF_lower66_5_Db, shp))
        call append(self%definition, reshape(self%config%soilmoisture4%PTF_higher66_5_constant, shp))
        call append(self%definition, reshape(self%config%soilmoisture4%PTF_higher66_5_clay, shp))
        call append(self%definition, reshape(self%config%soilmoisture4%PTF_higher66_5_Db, shp))
        call append(self%definition, reshape(self%config%soilmoisture4%PTF_Ks_constant, shp))
        call append(self%definition, reshape(self%config%soilmoisture4%PTF_Ks_sand, shp))
        call append(self%definition, reshape(self%config%soilmoisture4%PTF_Ks_clay, shp))
        call append(self%definition, reshape(self%config%soilmoisture4%PTF_Ks_curveSlope, shp))
        call append(self%definition, reshape(self%config%soilmoisture4%rootFractionCoefficient_forest, shp))
        call append(self%definition, reshape(self%config%soilmoisture4%rootFractionCoefficient_impervious, shp))
        call append(self%definition, reshape(self%config%soilmoisture4%rootFractionCoefficient_pervious, shp))
        call append(self%definition, reshape(self%config%soilmoisture4%infiltrationShapeFactor, shp))
        call append(self%definition, reshape(self%config%soilmoisture4%rootFractionCoefficient_sand, shp))
        call append(self%definition, reshape(self%config%soilmoisture4%rootFractionCoefficient_clay, shp))
        call append(self%definition, reshape(self%config%soilmoisture4%FCmin_glob, shp))
        call append(self%definition, reshape(self%config%soilmoisture4%FCdelta_glob, shp))

        call append(self%names, [     &
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
        if (.not. in_bound(self%definition)) &
          call error_message('***ERROR: parameter in namelist "soilmoisture4" out of bound in ', self%config%file)

      case DEFAULT
        call error_message('***ERROR: Process description for process "soilmoisture" does not exist!')
    end select

    ! Process 4 - sealed area directRunoff
    select case (self%process_matrix(4, 1))
      ! 1 - bucket exceedance approach
      case(1)
        if (self%config%directrunoff1%nml_status /= 0_i4) call error_message("'directrunoff1' namelist not found.")

        self%process_matrix(4, 2) = 1_i4
        self%process_matrix(4, 3) = sum(self%process_matrix(1 : 4, 2))
        call append(self%definition, reshape(self%config%directrunoff1%imperviousStorageCapacity, shp))

        call append(self%names, ['imperviousStorageCapacity'])

        ! check if parameter are in range
        if (.not. in_bound(self%definition)) &
          call error_message('***ERROR: parameter in namelist "directRunoff1" out of bound in ', self%config%file)

      case DEFAULT
        call error_message('***ERROR: Process description for process "directRunoff" does not exist!')
    end select

    ! Process 5 - potential evapotranspiration (PET)
    select case (self%process_matrix(5, 1))
      case(-1) ! 0 - PET is input, correct PET by LAI
        if (self%config%petminus1%nml_status /= 0_i4) call error_message("'petminus1' namelist not found.")

        self%process_matrix(5, 2) = 5_i4
        self%process_matrix(5, 3) = sum(self%process_matrix(1 : 5, 2))
        call append(self%definition, reshape(self%config%petminus1%PET_a_forest, shp))
        call append(self%definition, reshape(self%config%petminus1%PET_a_impervious, shp))
        call append(self%definition, reshape(self%config%petminus1%PET_a_pervious, shp))
        call append(self%definition, reshape(self%config%petminus1%PET_b, shp))
        call append(self%definition, reshape(self%config%petminus1%PET_c, shp))

        call append(self%names, [ &
                'PET_a_forest     ', &
                'PET_a_impervious ', &
                'PET_a_pervious   ', &
                'PET_b            ', &
                'PET_c            '])

        ! check if parameter are in range
        if (.not. in_bound(self%definition)) &
          call error_message('***ERROR: parameter in namelist "PETminus1" out of bound  n ', self%config%file)

      case(0) ! 0 - PET is input, correct PET by aspect
        if (self%config%pet0%nml_status /= 0_i4) call error_message("'pet0' namelist not found.")

        self%process_matrix(5, 2) = 3_i4
        self%process_matrix(5, 3) = sum(self%process_matrix(1 : 5, 2))
        call append(self%definition, reshape(self%config%pet0%minCorrectionFactorPET, shp))
        call append(self%definition, reshape(self%config%pet0%maxCorrectionFactorPET, shp))
        call append(self%definition, reshape(self%config%pet0%aspectTresholdPET, shp))

        call append(self%names, [ &
                'minCorrectionFactorPET ', &
                'maxCorrectionFactorPET ', &
                'aspectTresholdPET      '])

        ! check if parameter are in range
        if (.not. in_bound(self%definition)) &
          call error_message('***ERROR: parameter in namelist "PET0" out of bound in ', self%config%file)

      case(1) ! 1 - Hargreaves-Samani method (HarSam) - additional input needed: Tmin, Tmax
        if (self%config%pet1%nml_status /= 0_i4) call error_message("'pet1' namelist not found.")

        self%process_matrix(5, 2) = 4_i4
        self%process_matrix(5, 3) = sum(self%process_matrix(1 : 5, 2))
        call append(self%definition, reshape(self%config%pet1%minCorrectionFactorPET, shp))
        call append(self%definition, reshape(self%config%pet1%maxCorrectionFactorPET, shp))
        call append(self%definition, reshape(self%config%pet1%aspectTresholdPET, shp))
        call append(self%definition, reshape(self%config%pet1%HargreavesSamaniCoeff, shp))
        call append(self%names, [ &
                'minCorrectionFactorPET', &
                'maxCorrectionFactorPET', &
                'aspectTresholdPET     ', &
                'HargreavesSamaniCoeff '])

        ! check if parameter are in range
        if (.not. in_bound(self%definition)) &
          call error_message('***ERROR: parameter in namelist "PET1" out of bound in ', self%config%file)

      case(2) ! 2 - Priestley-Taylor method (PrieTay) - additional input needed: net_rad
        if (self%config%pet2%nml_status /= 0_i4) call error_message("'pet2' namelist not found.")

        self%process_matrix(5, 2) = 2_i4
        self%process_matrix(5, 3) = sum(self%process_matrix(1 : 5, 2))
        call append(self%definition, reshape(self%config%pet2%PriestleyTaylorCoeff, shp))
        call append(self%definition, reshape(self%config%pet2%PriestleyTaylorLAIcorr, shp))
        call append(self%names, [ &
                'PriestleyTaylorCoeff  ', &
                'PriestleyTaylorLAIcorr'])

        ! check if parameter are in range
        if (.not. in_bound(self%definition)) &
          call error_message('***ERROR: parameter in namelist "PET2" out of bound in ', self%config%file)

      case(3) ! 3 - Penman-Monteith method - additional input needed: net_rad, abs. vapour pressue, windspeed
        if (self%config%pet3%nml_status /= 0_i4) call error_message("'pet3' namelist not found.")

        self%process_matrix(5, 2) = 7_i4
        self%process_matrix(5, 3) = sum(self%process_matrix(1 : 5, 2))

        call append(self%definition, reshape(self%config%pet3%canopyheigth_forest, shp))
        call append(self%definition, reshape(self%config%pet3%canopyheigth_impervious, shp))
        call append(self%definition, reshape(self%config%pet3%canopyheigth_pervious, shp))
        call append(self%definition, reshape(self%config%pet3%displacementheight_coeff, shp))
        call append(self%definition, reshape(self%config%pet3%roughnesslength_momentum_coeff, shp))
        call append(self%definition, reshape(self%config%pet3%roughnesslength_heat_coeff, shp))
        call append(self%definition, reshape(self%config%pet3%stomatal_resistance, shp))

        call append(self%names, [ &
                'canopyheigth_forest           ', &
                'canopyheigth_impervious       ', &
                'canopyheigth_pervious         ', &
                'displacementheight_coeff      ', &
                'roughnesslength_momentum_coeff', &
                'roughnesslength_heat_coeff    ', &
                'stomatal_resistance           '])

        ! check if parameter are in range
        if (.not. in_bound(self%definition)) &
          call error_message('***ERROR: parameter in namelist "PET3" out of bound in ', self%config%file)

      case DEFAULT
        call error_message('***ERROR: Process description for process "actualET" does not exist!')
    end select

    ! Process 6 - interflow
    select case (self%process_matrix(6, 1))
      ! 1 - parallel soil reservoir approach
      case(1)
        if (self%config%interflow1%nml_status /= 0_i4) call error_message("'interflow1' namelist not found.")

        self%process_matrix(6, 2) = 5_i4
        self%process_matrix(6, 3) = sum(self%process_matrix(1 : 6, 2))
        call append(self%definition, reshape(self%config%interflow1%interflowStorageCapacityFactor, shp))
        call append(self%definition, reshape(self%config%interflow1%interflowRecession_slope, shp))
        call append(self%definition, reshape(self%config%interflow1%fastInterflowRecession_forest, shp))
        call append(self%definition, reshape(self%config%interflow1%slowInterflowRecession_Ks, shp))
        call append(self%definition, reshape(self%config%interflow1%exponentSlowInterflow, shp))

        call append(self%names, [ &
                'interflowStorageCapacityFactor', &
                'interflowRecession_slope      ', &
                'fastInterflowRecession_forest ', &
                'slowInterflowRecession_Ks     ', &
                'exponentSlowInterflow         '])

        ! check if parameter are in range
        if (.not. in_bound(self%definition)) &
          call error_message('***ERROR: parameter in namelist "interflow1" out of bound in ', self%config%file)

      case DEFAULT
        call error_message('***ERROR: Process description for process "interflow" does not exist!')
    end select

    ! Process 7 - percolation
    select case (self%process_matrix(7, 1))
      ! 1 - GW layer is assumed as bucket
      case(1)
        if (self%config%percolation1%nml_status /= 0_i4) call error_message("'percolation1' namelist not found.")

        self%process_matrix(7, 2) = 3_i4
        self%process_matrix(7, 3) = sum(self%process_matrix(1 : 7, 2))
        call append(self%definition, reshape(self%config%percolation1%rechargeCoefficient, shp))
        call append(self%definition, reshape(self%config%percolation1%rechargeFactor_karstic, shp))
        call append(self%definition, reshape(self%config%percolation1%gain_loss_GWreservoir_karstic, shp))

        call append(self%names, [ &
                'rechargeCoefficient          ', &
                'rechargeFactor_karstic       ', &
                'gain_loss_GWreservoir_karstic'])

        ! check if parameter are in range
        if (.not. in_bound(self%definition)) &
          call error_message('***ERROR: parameter in namelist "percolation1" out of bound in ', self%config%file)

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
        call append(self%definition, dummy_2d_dp)
        call append(self%names, ['dummy', 'dummy', 'dummy', 'dummy', 'dummy'])
      case(2)
        self%process_matrix(8, 2) = 1_i4
        self%process_matrix(8, 3) = sum(self%process_matrix(1 : 8, 2))
        call append(self%definition, dummy_2d_dp_2)
        call append(self%names, ['dummy'])
      case(3)
        self%process_matrix(8, 2) = 1_i4
        self%process_matrix(8, 3) = sum(self%process_matrix(1 : 8, 2))
        call append(self%definition, dummy_2d_dp_2)
        call append(self%names, ['dummy'])
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
        if (self%config%geoparameter%nml_status /= 0_i4) call error_message("'geoparameter' namelist not found.")
        GeoParam = self%config%geoparameter%GeoParam

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

        call append(self%definition, GeoParam(1 : self%nGeoUnits, :))

        ! create names
        do ii = 1, self%nGeoUnits
          dummy = 'GeoParam(' // trim(adjustl(num2str(ii))) // ',:)'
          call append(self%names, [ trim(dummy) ])
        end do

        ! check if parameter are in range
        if (.not. in_bound(self%definition)) &
          call error_message('***ERROR: parameter in namelist "geoparameter" out of bound in ', self%config%file)

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
        if (self%config%neutrons1%nml_status /= 0_i4) call error_message("'neutrons1' namelist not found.")

        self%process_matrix(10,2) = 3_i4
        self%process_matrix(10,3) = sum(self%process_matrix(1:10, 2))
        call append(self%definition, reshape(self%config%neutrons1%Desilets_N0,  shp))
        call append(self%definition, reshape(self%config%neutrons1%Desilets_LW0, shp))
        call append(self%definition, reshape(self%config%neutrons1%Desilets_LW1, shp))

        call append(self%names, [  &
                'Desilets_N0   ', &
                'Desilets_LW0  ', &
                'Desilets_LW1  '])

        ! check if parameter are in range
        if (.not. in_bound(self%definition)) &
          call error_message('***ERROR: parameter in namelist "neutrons1" out of bound in ', self%config%file)

      case(2)
        ! 2 - COSMIC version
        if (self%config%neutrons2%nml_status /= 0_i4) call error_message("'neutrons2' namelist not found.")

        self%process_matrix(10,2) = 9_i4
        self%process_matrix(10,3) = sum(self%process_matrix(1:10, 2))
        call append(self%definition, reshape(self%config%neutrons2%COSMIC_N0,     shp))
        call append(self%definition, reshape(self%config%neutrons2%COSMIC_N1,     shp))
        call append(self%definition, reshape(self%config%neutrons2%COSMIC_N2,     shp))
        call append(self%definition, reshape(self%config%neutrons2%COSMIC_alpha0, shp))
        call append(self%definition, reshape(self%config%neutrons2%COSMIC_alpha1, shp))
        call append(self%definition, reshape(self%config%neutrons2%COSMIC_L30,    shp))
        call append(self%definition, reshape(self%config%neutrons2%COSMIC_L31,    shp))
        call append(self%definition, reshape(self%config%neutrons2%COSMIC_LW0,    shp))
        call append(self%definition, reshape(self%config%neutrons2%COSMIC_LW1,    shp))

        call append(self%names, [  &
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
        if (.not. in_bound(self%definition)) &
          call error_message('***ERROR: parameter in namelist "neutrons2" out of bound in ', self%config%file)

      case DEFAULT
        call error_message('***ERROR: Process description for process "NEUTRON count" does not exist!')
    end select

    ! set default values
    self%values = self%definition(:, 3)

  end subroutine parameters_create

  !> set parameters from optimizer
  subroutine parameters_set(self,  parameters)
    class(parameters_t), intent(inout) :: self
    real(dp), dimension(:), optional, intent(in) :: parameters !< parameter values
    if (present(parameters)) then
      if (size(parameters) /= size(self%definition, dim=1)) call error_message("parameters%set: wrong number of parameters")
      self%values = parameters
    else
      self%values = self%definition(:, 3) ! default parameters from configuration
    end if
  end subroutine parameters_set

  !> get parameter by name
  real(dp) function parameters_get(self,  name)
    class(parameters_t), intent(in) :: self
    character(*), intent(in) ::  name !< parameter name
    integer(i4) :: i
    do i = 1_i4, size(self%names)
      if (trim(name) == trim(self%names(i))) then
        parameters_get = self%values(i)
        return
      end if
    end do
    call error_message("parameters%get: paramter '", trim(name), "' not present.")
  end function parameters_get

  !> get parameter array for selected process
  function parameters_get_process(self, id) result(array)
    class(parameters_t), intent(in) :: self
    integer(i4), intent(in) ::  id !< process id
    real(dp), allocatable :: array(:) !< resulting parameter array for selected process
    if (id < 1_i4 .or. id >= size(self%process_matrix, dim=1)) call error_message("parameters%get_process: id out of bounds.")
    if (self%process_matrix(id, 3_i4) == 0_i4) then
      allocate(array(0)) ! no parameters defined for this process
      return
    end if
    array = self%values(self%process_matrix(id, 3_i4) - self%process_matrix(id, 2_i4) + 1_i4 : self%process_matrix(id, 3_i4))
  end function parameters_get_process

  !> set parameters from optimizer
  subroutine parameters_print(self)
    class(parameters_t), intent(inout) :: self
    integer(i4) :: i
    do i = 1_i4, size(self%names)
      call message(self%names(i)(1:35), n2s(self%values(i)))
    end do
  end subroutine parameters_print

end module mo_main_config
