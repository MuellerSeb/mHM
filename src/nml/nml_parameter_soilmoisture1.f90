!> \file nml_parameter_soilmoisture1.f90
!> \copydoc nml_soilmoisture1

!> \brief Soil moisture - Case 1
!> \details Feddes equation for ET reduction, multi-layer infiltration capacity approach, Brooks-Corey like
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Jan 2026
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_namelists
module nml_soilmoisture1
  use nml_helper, only: &
    nml_file_t, &
    nml_line_buffer, &
    NML_OK, &
    NML_ERR_FILE_NOT_FOUND, &
    NML_ERR_OPEN, &
    NML_ERR_NOT_OPEN, &
    NML_ERR_NML_NOT_FOUND, &
    NML_ERR_READ, &
    NML_ERR_CLOSE, &
    NML_ERR_REQUIRED, &
    NML_ERR_ENUM, &
    NML_ERR_BOUNDS, &
    NML_ERR_NOT_SET, &
    NML_ERR_INVALID_NAME, &
    NML_ERR_INVALID_INDEX, &
    idx_check, &
    NML_ERR_PARTLY_SET
  use ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_is_nan
  ! kind specifiers listed in the nml-tools configuration file
  use mo_kind, only: &
    dp

  implicit none

  ! default values
  real(dp), parameter, public :: PTF_Ks_curveSlope_default(5) = [60.96_dp, 60.96_dp, 60.96_dp, 0.0_dp, 1.0_dp]

  !> \class nml_soilmoisture1_t
  !> \brief Soil moisture - Case 1
  !> \details Feddes equation for ET reduction, multi-layer infiltration capacity approach, Brooks-Corey like
  type, public :: nml_soilmoisture1_t
    logical :: is_configured = .false. !< whether the namelist has been configured
    real(dp), dimension(5) :: orgMatterContent_forest !< Organic matter content for forest
    real(dp), dimension(5) :: orgMatterContent_impervious !< Organic matter content for impervious
    real(dp), dimension(5) :: orgMatterContent_pervious !< Organic matter content for pervious
    real(dp), dimension(5) :: PTF_lower66_5_constant !< Zacharias PTF parameters below 66.5 % sand content
    real(dp), dimension(5) :: PTF_lower66_5_clay !< Multiplier for clay constant below 66.5 % sand content
    real(dp), dimension(5) :: PTF_lower66_5_Db !< Multiplier for mineral bulk density below 66.5 % sand content
    real(dp), dimension(5) :: PTF_higher66_5_constant !< Zacharias PTF parameters above 66.5 % sand content
    real(dp), dimension(5) :: PTF_higher66_5_clay !< Multiplier for clay constant above 66.5 % sand content
    real(dp), dimension(5) :: PTF_higher66_5_Db !< Multiplier for mineral bulk density above 66.5 % sand content
    real(dp), dimension(5) :: PTF_Ks_constant !< PTF constant for saturated hydraulic conductivity
    real(dp), dimension(5) :: PTF_Ks_sand !< Multiplier for sand for saturated hydraulic conductivity
    real(dp), dimension(5) :: PTF_Ks_clay !< Multiplier for clay for saturated hydraulic conductivity
    real(dp), dimension(5) :: PTF_Ks_curveSlope !< Unit conversion factor
    real(dp), dimension(5) :: rootFractionCoefficient_forest !< Root fraction coefficient for forest
    real(dp), dimension(5) :: rootFractionCoefficient_impervious !< Root fraction coefficient for impervious
    real(dp), dimension(5) :: rootFractionCoefficient_pervious !< Root fraction coefficient for pervious
    real(dp), dimension(5) :: infiltrationShapeFactor !< Infiltration shape factor
  contains
    procedure :: init => nml_soilmoisture1_init
    procedure :: from_file => nml_soilmoisture1_from_file
    procedure :: set => nml_soilmoisture1_set
    procedure :: is_set => nml_soilmoisture1_is_set
    procedure :: is_valid => nml_soilmoisture1_is_valid
  end type nml_soilmoisture1_t

contains

  !> \brief Initialize defaults and sentinels for soilmoisture1
  integer function nml_soilmoisture1_init(this, errmsg) result(status)
    class(nml_soilmoisture1_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    this%is_configured = .false.

    ! sentinel values for required/optional parameters
    this%orgMatterContent_forest = ieee_value(this%orgMatterContent_forest, ieee_quiet_nan) ! sentinel for required real array
    this%orgMatterContent_impervious = ieee_value(this%orgMatterContent_impervious, ieee_quiet_nan) ! sentinel for required real array
    this%orgMatterContent_pervious = ieee_value(this%orgMatterContent_pervious, ieee_quiet_nan) ! sentinel for required real array
    this%PTF_lower66_5_constant = ieee_value(this%PTF_lower66_5_constant, ieee_quiet_nan) ! sentinel for required real array
    this%PTF_lower66_5_clay = ieee_value(this%PTF_lower66_5_clay, ieee_quiet_nan) ! sentinel for required real array
    this%PTF_lower66_5_Db = ieee_value(this%PTF_lower66_5_Db, ieee_quiet_nan) ! sentinel for required real array
    this%PTF_higher66_5_constant = ieee_value(this%PTF_higher66_5_constant, ieee_quiet_nan) ! sentinel for required real array
    this%PTF_higher66_5_clay = ieee_value(this%PTF_higher66_5_clay, ieee_quiet_nan) ! sentinel for required real array
    this%PTF_higher66_5_Db = ieee_value(this%PTF_higher66_5_Db, ieee_quiet_nan) ! sentinel for required real array
    this%PTF_Ks_constant = ieee_value(this%PTF_Ks_constant, ieee_quiet_nan) ! sentinel for required real array
    this%PTF_Ks_sand = ieee_value(this%PTF_Ks_sand, ieee_quiet_nan) ! sentinel for required real array
    this%PTF_Ks_clay = ieee_value(this%PTF_Ks_clay, ieee_quiet_nan) ! sentinel for required real array
    this%rootFractionCoefficient_forest = ieee_value(this%rootFractionCoefficient_forest, ieee_quiet_nan) ! sentinel for required real array
    this%rootFractionCoefficient_impervious = ieee_value(this%rootFractionCoefficient_impervious, ieee_quiet_nan) ! sentinel for required real array
    this%rootFractionCoefficient_pervious = ieee_value(this%rootFractionCoefficient_pervious, ieee_quiet_nan) ! sentinel for required real array
    this%infiltrationShapeFactor = ieee_value(this%infiltrationShapeFactor, ieee_quiet_nan) ! sentinel for required real array
    ! default values
    this%PTF_Ks_curveSlope = PTF_Ks_curveSlope_default
  end function nml_soilmoisture1_init

  !> \brief Read soilmoisture1 namelist from file
  integer function nml_soilmoisture1_from_file(this, file, errmsg) result(status)
    class(nml_soilmoisture1_t), intent(inout) :: this
    character(len=*), intent(in) :: file !< path to namelist file
    character(len=*), intent(out), optional :: errmsg
    ! namelist variables
    real(dp), dimension(5) :: orgMatterContent_forest
    real(dp), dimension(5) :: orgMatterContent_impervious
    real(dp), dimension(5) :: orgMatterContent_pervious
    real(dp), dimension(5) :: PTF_lower66_5_constant
    real(dp), dimension(5) :: PTF_lower66_5_clay
    real(dp), dimension(5) :: PTF_lower66_5_Db
    real(dp), dimension(5) :: PTF_higher66_5_constant
    real(dp), dimension(5) :: PTF_higher66_5_clay
    real(dp), dimension(5) :: PTF_higher66_5_Db
    real(dp), dimension(5) :: PTF_Ks_constant
    real(dp), dimension(5) :: PTF_Ks_sand
    real(dp), dimension(5) :: PTF_Ks_clay
    real(dp), dimension(5) :: PTF_Ks_curveSlope
    real(dp), dimension(5) :: rootFractionCoefficient_forest
    real(dp), dimension(5) :: rootFractionCoefficient_impervious
    real(dp), dimension(5) :: rootFractionCoefficient_pervious
    real(dp), dimension(5) :: infiltrationShapeFactor
    ! locals
    type(nml_file_t) :: nml
    integer :: iostat
    integer :: close_status
    character(len=nml_line_buffer) :: iomsg

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

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return
    orgMatterContent_forest = this%orgMatterContent_forest
    orgMatterContent_impervious = this%orgMatterContent_impervious
    orgMatterContent_pervious = this%orgMatterContent_pervious
    PTF_lower66_5_constant = this%PTF_lower66_5_constant
    PTF_lower66_5_clay = this%PTF_lower66_5_clay
    PTF_lower66_5_Db = this%PTF_lower66_5_Db
    PTF_higher66_5_constant = this%PTF_higher66_5_constant
    PTF_higher66_5_clay = this%PTF_higher66_5_clay
    PTF_higher66_5_Db = this%PTF_higher66_5_Db
    PTF_Ks_constant = this%PTF_Ks_constant
    PTF_Ks_sand = this%PTF_Ks_sand
    PTF_Ks_clay = this%PTF_Ks_clay
    PTF_Ks_curveSlope = this%PTF_Ks_curveSlope
    rootFractionCoefficient_forest = this%rootFractionCoefficient_forest
    rootFractionCoefficient_impervious = this%rootFractionCoefficient_impervious
    rootFractionCoefficient_pervious = this%rootFractionCoefficient_pervious
    infiltrationShapeFactor = this%infiltrationShapeFactor

    status = nml%open(file, errmsg=errmsg)
    if (status /= NML_OK) return

    status = nml%find("soilmoisture1", errmsg=errmsg)
    if (status /= NML_OK) then
      close_status = nml%close()
      return
    end if

    ! read namelist
    read(nml%unit, nml=soilmoisture1, iostat=iostat, iomsg=iomsg)
    if (iostat /= 0) then
      status = NML_ERR_READ
      if (present(errmsg)) errmsg = trim(iomsg)
      close_status = nml%close()
      return
    end if
    close_status = nml%close(errmsg=errmsg)
    if (close_status /= NML_OK) then
      status = close_status
      return
    end if

    ! assign values
    this%orgMatterContent_forest = orgMatterContent_forest
    this%orgMatterContent_impervious = orgMatterContent_impervious
    this%orgMatterContent_pervious = orgMatterContent_pervious
    this%PTF_lower66_5_constant = PTF_lower66_5_constant
    this%PTF_lower66_5_clay = PTF_lower66_5_clay
    this%PTF_lower66_5_Db = PTF_lower66_5_Db
    this%PTF_higher66_5_constant = PTF_higher66_5_constant
    this%PTF_higher66_5_clay = PTF_higher66_5_clay
    this%PTF_higher66_5_Db = PTF_higher66_5_Db
    this%PTF_Ks_constant = PTF_Ks_constant
    this%PTF_Ks_sand = PTF_Ks_sand
    this%PTF_Ks_clay = PTF_Ks_clay
    this%PTF_Ks_curveSlope = PTF_Ks_curveSlope
    this%rootFractionCoefficient_forest = rootFractionCoefficient_forest
    this%rootFractionCoefficient_impervious = rootFractionCoefficient_impervious
    this%rootFractionCoefficient_pervious = rootFractionCoefficient_pervious
    this%infiltrationShapeFactor = infiltrationShapeFactor

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_soilmoisture1_from_file

  !> \brief Set soilmoisture1 values
  integer function nml_soilmoisture1_set(this, &
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
    rootFractionCoefficient_forest, &
    rootFractionCoefficient_impervious, &
    rootFractionCoefficient_pervious, &
    infiltrationShapeFactor, &
    PTF_Ks_curveSlope, &
    errmsg) result(status)

    class(nml_soilmoisture1_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg
    real(dp), dimension(5), intent(in) :: orgMatterContent_forest
    real(dp), dimension(5), intent(in) :: orgMatterContent_impervious
    real(dp), dimension(5), intent(in) :: orgMatterContent_pervious
    real(dp), dimension(5), intent(in) :: PTF_lower66_5_constant
    real(dp), dimension(5), intent(in) :: PTF_lower66_5_clay
    real(dp), dimension(5), intent(in) :: PTF_lower66_5_Db
    real(dp), dimension(5), intent(in) :: PTF_higher66_5_constant
    real(dp), dimension(5), intent(in) :: PTF_higher66_5_clay
    real(dp), dimension(5), intent(in) :: PTF_higher66_5_Db
    real(dp), dimension(5), intent(in) :: PTF_Ks_constant
    real(dp), dimension(5), intent(in) :: PTF_Ks_sand
    real(dp), dimension(5), intent(in) :: PTF_Ks_clay
    real(dp), dimension(5), intent(in) :: rootFractionCoefficient_forest
    real(dp), dimension(5), intent(in) :: rootFractionCoefficient_impervious
    real(dp), dimension(5), intent(in) :: rootFractionCoefficient_pervious
    real(dp), dimension(5), intent(in) :: infiltrationShapeFactor
    real(dp), dimension(:), intent(in), optional :: PTF_Ks_curveSlope
    integer :: &
      lb_1, &
      ub_1

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return

    ! required parameters
    this%orgMatterContent_forest = orgMatterContent_forest
    this%orgMatterContent_impervious = orgMatterContent_impervious
    this%orgMatterContent_pervious = orgMatterContent_pervious
    this%PTF_lower66_5_constant = PTF_lower66_5_constant
    this%PTF_lower66_5_clay = PTF_lower66_5_clay
    this%PTF_lower66_5_Db = PTF_lower66_5_Db
    this%PTF_higher66_5_constant = PTF_higher66_5_constant
    this%PTF_higher66_5_clay = PTF_higher66_5_clay
    this%PTF_higher66_5_Db = PTF_higher66_5_Db
    this%PTF_Ks_constant = PTF_Ks_constant
    this%PTF_Ks_sand = PTF_Ks_sand
    this%PTF_Ks_clay = PTF_Ks_clay
    this%rootFractionCoefficient_forest = rootFractionCoefficient_forest
    this%rootFractionCoefficient_impervious = rootFractionCoefficient_impervious
    this%rootFractionCoefficient_pervious = rootFractionCoefficient_pervious
    this%infiltrationShapeFactor = infiltrationShapeFactor
    ! override with provided values
    if (present(PTF_Ks_curveSlope)) then
      if (size(PTF_Ks_curveSlope, 1) > size(this%PTF_Ks_curveSlope, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'PTF_Ks_curveSlope'"
        return
      end if
      lb_1 = lbound(this%PTF_Ks_curveSlope, 1)
      ub_1 = lb_1 + size(PTF_Ks_curveSlope, 1) - 1
      this%PTF_Ks_curveSlope(lb_1:ub_1) = PTF_Ks_curveSlope
    end if

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_soilmoisture1_set

  !> \brief Check whether a namelist value was set
  integer function nml_soilmoisture1_is_set(this, name, idx, errmsg) result(status)
    class(nml_soilmoisture1_t), intent(in) :: this
    character(len=*), intent(in) :: name
    integer, intent(in), optional :: idx(:)
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    select case (trim(name))
    case ("orgMatterContent_forest")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%orgMatterContent_forest), ubound(this%orgMatterContent_forest), &
          "orgMatterContent_forest", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%orgMatterContent_forest(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%orgMatterContent_forest))) status = NML_ERR_NOT_SET
      end if
    case ("orgMatterContent_impervious")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%orgMatterContent_impervious), ubound(this%orgMatterContent_impervious), &
          "orgMatterContent_impervious", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%orgMatterContent_impervious(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%orgMatterContent_impervious))) status = NML_ERR_NOT_SET
      end if
    case ("orgMatterContent_pervious")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%orgMatterContent_pervious), ubound(this%orgMatterContent_pervious), &
          "orgMatterContent_pervious", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%orgMatterContent_pervious(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%orgMatterContent_pervious))) status = NML_ERR_NOT_SET
      end if
    case ("PTF_lower66_5_constant")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%PTF_lower66_5_constant), ubound(this%PTF_lower66_5_constant), &
          "PTF_lower66_5_constant", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%PTF_lower66_5_constant(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%PTF_lower66_5_constant))) status = NML_ERR_NOT_SET
      end if
    case ("PTF_lower66_5_clay")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%PTF_lower66_5_clay), ubound(this%PTF_lower66_5_clay), &
          "PTF_lower66_5_clay", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%PTF_lower66_5_clay(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%PTF_lower66_5_clay))) status = NML_ERR_NOT_SET
      end if
    case ("PTF_lower66_5_Db")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%PTF_lower66_5_Db), ubound(this%PTF_lower66_5_Db), &
          "PTF_lower66_5_Db", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%PTF_lower66_5_Db(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%PTF_lower66_5_Db))) status = NML_ERR_NOT_SET
      end if
    case ("PTF_higher66_5_constant")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%PTF_higher66_5_constant), ubound(this%PTF_higher66_5_constant), &
          "PTF_higher66_5_constant", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%PTF_higher66_5_constant(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%PTF_higher66_5_constant))) status = NML_ERR_NOT_SET
      end if
    case ("PTF_higher66_5_clay")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%PTF_higher66_5_clay), ubound(this%PTF_higher66_5_clay), &
          "PTF_higher66_5_clay", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%PTF_higher66_5_clay(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%PTF_higher66_5_clay))) status = NML_ERR_NOT_SET
      end if
    case ("PTF_higher66_5_Db")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%PTF_higher66_5_Db), ubound(this%PTF_higher66_5_Db), &
          "PTF_higher66_5_Db", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%PTF_higher66_5_Db(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%PTF_higher66_5_Db))) status = NML_ERR_NOT_SET
      end if
    case ("PTF_Ks_constant")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%PTF_Ks_constant), ubound(this%PTF_Ks_constant), &
          "PTF_Ks_constant", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%PTF_Ks_constant(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%PTF_Ks_constant))) status = NML_ERR_NOT_SET
      end if
    case ("PTF_Ks_sand")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%PTF_Ks_sand), ubound(this%PTF_Ks_sand), &
          "PTF_Ks_sand", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%PTF_Ks_sand(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%PTF_Ks_sand))) status = NML_ERR_NOT_SET
      end if
    case ("PTF_Ks_clay")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%PTF_Ks_clay), ubound(this%PTF_Ks_clay), &
          "PTF_Ks_clay", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%PTF_Ks_clay(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%PTF_Ks_clay))) status = NML_ERR_NOT_SET
      end if
    case ("PTF_Ks_curveSlope")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%PTF_Ks_curveSlope), ubound(this%PTF_Ks_curveSlope), &
          "PTF_Ks_curveSlope", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("rootFractionCoefficient_forest")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%rootFractionCoefficient_forest), ubound(this%rootFractionCoefficient_forest), &
          "rootFractionCoefficient_forest", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%rootFractionCoefficient_forest(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%rootFractionCoefficient_forest))) status = NML_ERR_NOT_SET
      end if
    case ("rootFractionCoefficient_impervious")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%rootFractionCoefficient_impervious), ubound(this%rootFractionCoefficient_impervious), &
          "rootFractionCoefficient_impervious", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%rootFractionCoefficient_impervious(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%rootFractionCoefficient_impervious))) status = NML_ERR_NOT_SET
      end if
    case ("rootFractionCoefficient_pervious")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%rootFractionCoefficient_pervious), ubound(this%rootFractionCoefficient_pervious), &
          "rootFractionCoefficient_pervious", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%rootFractionCoefficient_pervious(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%rootFractionCoefficient_pervious))) status = NML_ERR_NOT_SET
      end if
    case ("infiltrationShapeFactor")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%infiltrationShapeFactor), ubound(this%infiltrationShapeFactor), &
          "infiltrationShapeFactor", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%infiltrationShapeFactor(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%infiltrationShapeFactor))) status = NML_ERR_NOT_SET
      end if
    case default
      status = NML_ERR_INVALID_NAME
      if (present(errmsg)) errmsg = "unknown field: " // trim(name)
    end select
    if (status == NML_ERR_NOT_SET .and. present(errmsg)) then
      if (len_trim(errmsg) == 0) errmsg = "field not set: " // trim(name)
    end if
  end function nml_soilmoisture1_is_set

  !> \brief Validate required values and constraints
  integer function nml_soilmoisture1_is_valid(this, errmsg) result(status)
    class(nml_soilmoisture1_t), intent(in) :: this
    character(len=*), intent(out), optional :: errmsg
    integer :: istat

    status = NML_OK
    if (present(errmsg)) errmsg = ""

    ! required arrays
    if (all(ieee_is_nan(this%orgMatterContent_forest))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: orgMatterContent_forest"
      return
    end if
    if (any(ieee_is_nan(this%orgMatterContent_forest))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: orgMatterContent_forest"
      return
    end if
    if (all(ieee_is_nan(this%orgMatterContent_impervious))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: orgMatterContent_impervious"
      return
    end if
    if (any(ieee_is_nan(this%orgMatterContent_impervious))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: orgMatterContent_impervious"
      return
    end if
    if (all(ieee_is_nan(this%orgMatterContent_pervious))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: orgMatterContent_pervious"
      return
    end if
    if (any(ieee_is_nan(this%orgMatterContent_pervious))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: orgMatterContent_pervious"
      return
    end if
    if (all(ieee_is_nan(this%PTF_lower66_5_constant))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: PTF_lower66_5_constant"
      return
    end if
    if (any(ieee_is_nan(this%PTF_lower66_5_constant))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: PTF_lower66_5_constant"
      return
    end if
    if (all(ieee_is_nan(this%PTF_lower66_5_clay))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: PTF_lower66_5_clay"
      return
    end if
    if (any(ieee_is_nan(this%PTF_lower66_5_clay))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: PTF_lower66_5_clay"
      return
    end if
    if (all(ieee_is_nan(this%PTF_lower66_5_Db))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: PTF_lower66_5_Db"
      return
    end if
    if (any(ieee_is_nan(this%PTF_lower66_5_Db))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: PTF_lower66_5_Db"
      return
    end if
    if (all(ieee_is_nan(this%PTF_higher66_5_constant))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: PTF_higher66_5_constant"
      return
    end if
    if (any(ieee_is_nan(this%PTF_higher66_5_constant))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: PTF_higher66_5_constant"
      return
    end if
    if (all(ieee_is_nan(this%PTF_higher66_5_clay))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: PTF_higher66_5_clay"
      return
    end if
    if (any(ieee_is_nan(this%PTF_higher66_5_clay))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: PTF_higher66_5_clay"
      return
    end if
    if (all(ieee_is_nan(this%PTF_higher66_5_Db))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: PTF_higher66_5_Db"
      return
    end if
    if (any(ieee_is_nan(this%PTF_higher66_5_Db))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: PTF_higher66_5_Db"
      return
    end if
    if (all(ieee_is_nan(this%PTF_Ks_constant))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: PTF_Ks_constant"
      return
    end if
    if (any(ieee_is_nan(this%PTF_Ks_constant))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: PTF_Ks_constant"
      return
    end if
    if (all(ieee_is_nan(this%PTF_Ks_sand))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: PTF_Ks_sand"
      return
    end if
    if (any(ieee_is_nan(this%PTF_Ks_sand))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: PTF_Ks_sand"
      return
    end if
    if (all(ieee_is_nan(this%PTF_Ks_clay))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: PTF_Ks_clay"
      return
    end if
    if (any(ieee_is_nan(this%PTF_Ks_clay))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: PTF_Ks_clay"
      return
    end if
    if (all(ieee_is_nan(this%rootFractionCoefficient_forest))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: rootFractionCoefficient_forest"
      return
    end if
    if (any(ieee_is_nan(this%rootFractionCoefficient_forest))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: rootFractionCoefficient_forest"
      return
    end if
    if (all(ieee_is_nan(this%rootFractionCoefficient_impervious))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: rootFractionCoefficient_impervious"
      return
    end if
    if (any(ieee_is_nan(this%rootFractionCoefficient_impervious))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: rootFractionCoefficient_impervious"
      return
    end if
    if (all(ieee_is_nan(this%rootFractionCoefficient_pervious))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: rootFractionCoefficient_pervious"
      return
    end if
    if (any(ieee_is_nan(this%rootFractionCoefficient_pervious))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: rootFractionCoefficient_pervious"
      return
    end if
    if (all(ieee_is_nan(this%infiltrationShapeFactor))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: infiltrationShapeFactor"
      return
    end if
    if (any(ieee_is_nan(this%infiltrationShapeFactor))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: infiltrationShapeFactor"
      return
    end if
  end function nml_soilmoisture1_is_valid

end module nml_soilmoisture1
