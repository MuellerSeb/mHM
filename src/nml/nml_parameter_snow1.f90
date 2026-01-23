!> \file nml_parameter_snow1.f90
!> \copydoc nml_snow1

!> \brief Snow - Case 1
!> \details Parameters for Snow module.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Jan 2026
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_namelists
module nml_snow1
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

  !> \class nml_snow1_t
  !> \brief Snow - Case 1
  !> \details Parameters for Snow module.
  type, public :: nml_snow1_t
    logical :: is_configured = .false. !< whether the namelist has been configured
    real(dp), dimension(5) :: snowTreshholdTemperature !< Threshold for rain/snow partitioning [degC].
    real(dp), dimension(5) :: degreeDayFactor_forest !< Degree day factors to determine melting flux [m degC-1].
    real(dp), dimension(5) :: degreeDayFactor_impervious !< Degree day factors to determine melting flux [m degC-1].
    real(dp), dimension(5) :: degreeDayFactor_pervious !< Degree day factors to determine melting flux [m degC-1].
    real(dp), dimension(5) :: increaseDegreeDayFactorByPrecip !< Increase of degree day factor if there is precipitation [degC-1].
    real(dp), dimension(5) :: maxDegreeDayFactor_forest !< Maximum values for degree day factor [m degC-1].
    real(dp), dimension(5) :: maxDegreeDayFactor_impervious !< Maximum values for degree day factor [m degC-1].
    real(dp), dimension(5) :: maxDegreeDayFactor_pervious !< Maximum values for degree day factor [m degC-1].
  contains
    procedure :: init => nml_snow1_init
    procedure :: from_file => nml_snow1_from_file
    procedure :: set => nml_snow1_set
    procedure :: is_set => nml_snow1_is_set
    procedure :: is_valid => nml_snow1_is_valid
  end type nml_snow1_t

contains

  !> \brief Initialize defaults and sentinels for snow1
  integer function nml_snow1_init(this, errmsg) result(status)
    class(nml_snow1_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    this%is_configured = .false.

    ! sentinel values for required/optional parameters
    this%snowTreshholdTemperature = ieee_value(this%snowTreshholdTemperature, ieee_quiet_nan) ! sentinel for required real array
    this%degreeDayFactor_forest = ieee_value(this%degreeDayFactor_forest, ieee_quiet_nan) ! sentinel for required real array
    this%degreeDayFactor_impervious = ieee_value(this%degreeDayFactor_impervious, ieee_quiet_nan) ! sentinel for required real array
    this%degreeDayFactor_pervious = ieee_value(this%degreeDayFactor_pervious, ieee_quiet_nan) ! sentinel for required real array
    this%increaseDegreeDayFactorByPrecip = ieee_value(this%increaseDegreeDayFactorByPrecip, ieee_quiet_nan) ! sentinel for required real array
    this%maxDegreeDayFactor_forest = ieee_value(this%maxDegreeDayFactor_forest, ieee_quiet_nan) ! sentinel for required real array
    this%maxDegreeDayFactor_impervious = ieee_value(this%maxDegreeDayFactor_impervious, ieee_quiet_nan) ! sentinel for required real array
    this%maxDegreeDayFactor_pervious = ieee_value(this%maxDegreeDayFactor_pervious, ieee_quiet_nan) ! sentinel for required real array
  end function nml_snow1_init

  !> \brief Read snow1 namelist from file
  integer function nml_snow1_from_file(this, file, errmsg) result(status)
    class(nml_snow1_t), intent(inout) :: this
    character(len=*), intent(in) :: file !< path to namelist file
    character(len=*), intent(out), optional :: errmsg
    ! namelist variables
    real(dp), dimension(5) :: snowTreshholdTemperature
    real(dp), dimension(5) :: degreeDayFactor_forest
    real(dp), dimension(5) :: degreeDayFactor_impervious
    real(dp), dimension(5) :: degreeDayFactor_pervious
    real(dp), dimension(5) :: increaseDegreeDayFactorByPrecip
    real(dp), dimension(5) :: maxDegreeDayFactor_forest
    real(dp), dimension(5) :: maxDegreeDayFactor_impervious
    real(dp), dimension(5) :: maxDegreeDayFactor_pervious
    ! locals
    type(nml_file_t) :: nml
    integer :: iostat
    integer :: close_status
    character(len=nml_line_buffer) :: iomsg

    namelist /snow1/ &
      snowTreshholdTemperature, &
      degreeDayFactor_forest, &
      degreeDayFactor_impervious, &
      degreeDayFactor_pervious, &
      increaseDegreeDayFactorByPrecip, &
      maxDegreeDayFactor_forest, &
      maxDegreeDayFactor_impervious, &
      maxDegreeDayFactor_pervious

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return
    snowTreshholdTemperature = this%snowTreshholdTemperature
    degreeDayFactor_forest = this%degreeDayFactor_forest
    degreeDayFactor_impervious = this%degreeDayFactor_impervious
    degreeDayFactor_pervious = this%degreeDayFactor_pervious
    increaseDegreeDayFactorByPrecip = this%increaseDegreeDayFactorByPrecip
    maxDegreeDayFactor_forest = this%maxDegreeDayFactor_forest
    maxDegreeDayFactor_impervious = this%maxDegreeDayFactor_impervious
    maxDegreeDayFactor_pervious = this%maxDegreeDayFactor_pervious

    status = nml%open(file, errmsg=errmsg)
    if (status /= NML_OK) return

    status = nml%find("snow1", errmsg=errmsg)
    if (status /= NML_OK) then
      close_status = nml%close()
      return
    end if

    ! read namelist
    read(nml%unit, nml=snow1, iostat=iostat, iomsg=iomsg)
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
    this%snowTreshholdTemperature = snowTreshholdTemperature
    this%degreeDayFactor_forest = degreeDayFactor_forest
    this%degreeDayFactor_impervious = degreeDayFactor_impervious
    this%degreeDayFactor_pervious = degreeDayFactor_pervious
    this%increaseDegreeDayFactorByPrecip = increaseDegreeDayFactorByPrecip
    this%maxDegreeDayFactor_forest = maxDegreeDayFactor_forest
    this%maxDegreeDayFactor_impervious = maxDegreeDayFactor_impervious
    this%maxDegreeDayFactor_pervious = maxDegreeDayFactor_pervious

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_snow1_from_file

  !> \brief Set snow1 values
  integer function nml_snow1_set(this, &
    snowTreshholdTemperature, &
    degreeDayFactor_forest, &
    degreeDayFactor_impervious, &
    degreeDayFactor_pervious, &
    increaseDegreeDayFactorByPrecip, &
    maxDegreeDayFactor_forest, &
    maxDegreeDayFactor_impervious, &
    maxDegreeDayFactor_pervious, &
    errmsg) result(status)

    class(nml_snow1_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg
    real(dp), dimension(5), intent(in) :: snowTreshholdTemperature
    real(dp), dimension(5), intent(in) :: degreeDayFactor_forest
    real(dp), dimension(5), intent(in) :: degreeDayFactor_impervious
    real(dp), dimension(5), intent(in) :: degreeDayFactor_pervious
    real(dp), dimension(5), intent(in) :: increaseDegreeDayFactorByPrecip
    real(dp), dimension(5), intent(in) :: maxDegreeDayFactor_forest
    real(dp), dimension(5), intent(in) :: maxDegreeDayFactor_impervious
    real(dp), dimension(5), intent(in) :: maxDegreeDayFactor_pervious

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return

    ! required parameters
    this%snowTreshholdTemperature = snowTreshholdTemperature
    this%degreeDayFactor_forest = degreeDayFactor_forest
    this%degreeDayFactor_impervious = degreeDayFactor_impervious
    this%degreeDayFactor_pervious = degreeDayFactor_pervious
    this%increaseDegreeDayFactorByPrecip = increaseDegreeDayFactorByPrecip
    this%maxDegreeDayFactor_forest = maxDegreeDayFactor_forest
    this%maxDegreeDayFactor_impervious = maxDegreeDayFactor_impervious
    this%maxDegreeDayFactor_pervious = maxDegreeDayFactor_pervious

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_snow1_set

  !> \brief Check whether a namelist value was set
  integer function nml_snow1_is_set(this, name, idx, errmsg) result(status)
    class(nml_snow1_t), intent(in) :: this
    character(len=*), intent(in) :: name
    integer, intent(in), optional :: idx(:)
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    select case (trim(name))
    case ("snowTreshholdTemperature")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%snowTreshholdTemperature), ubound(this%snowTreshholdTemperature), &
          "snowTreshholdTemperature", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%snowTreshholdTemperature(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%snowTreshholdTemperature))) status = NML_ERR_NOT_SET
      end if
    case ("degreeDayFactor_forest")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%degreeDayFactor_forest), ubound(this%degreeDayFactor_forest), &
          "degreeDayFactor_forest", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%degreeDayFactor_forest(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%degreeDayFactor_forest))) status = NML_ERR_NOT_SET
      end if
    case ("degreeDayFactor_impervious")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%degreeDayFactor_impervious), ubound(this%degreeDayFactor_impervious), &
          "degreeDayFactor_impervious", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%degreeDayFactor_impervious(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%degreeDayFactor_impervious))) status = NML_ERR_NOT_SET
      end if
    case ("degreeDayFactor_pervious")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%degreeDayFactor_pervious), ubound(this%degreeDayFactor_pervious), &
          "degreeDayFactor_pervious", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%degreeDayFactor_pervious(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%degreeDayFactor_pervious))) status = NML_ERR_NOT_SET
      end if
    case ("increaseDegreeDayFactorByPrecip")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%increaseDegreeDayFactorByPrecip), ubound(this%increaseDegreeDayFactorByPrecip), &
          "increaseDegreeDayFactorByPrecip", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%increaseDegreeDayFactorByPrecip(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%increaseDegreeDayFactorByPrecip))) status = NML_ERR_NOT_SET
      end if
    case ("maxDegreeDayFactor_forest")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%maxDegreeDayFactor_forest), ubound(this%maxDegreeDayFactor_forest), &
          "maxDegreeDayFactor_forest", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%maxDegreeDayFactor_forest(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%maxDegreeDayFactor_forest))) status = NML_ERR_NOT_SET
      end if
    case ("maxDegreeDayFactor_impervious")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%maxDegreeDayFactor_impervious), ubound(this%maxDegreeDayFactor_impervious), &
          "maxDegreeDayFactor_impervious", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%maxDegreeDayFactor_impervious(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%maxDegreeDayFactor_impervious))) status = NML_ERR_NOT_SET
      end if
    case ("maxDegreeDayFactor_pervious")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%maxDegreeDayFactor_pervious), ubound(this%maxDegreeDayFactor_pervious), &
          "maxDegreeDayFactor_pervious", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%maxDegreeDayFactor_pervious(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%maxDegreeDayFactor_pervious))) status = NML_ERR_NOT_SET
      end if
    case default
      status = NML_ERR_INVALID_NAME
      if (present(errmsg)) errmsg = "unknown field: " // trim(name)
    end select
    if (status == NML_ERR_NOT_SET .and. present(errmsg)) then
      if (len_trim(errmsg) == 0) errmsg = "field not set: " // trim(name)
    end if
  end function nml_snow1_is_set

  !> \brief Validate required values and constraints
  integer function nml_snow1_is_valid(this, errmsg) result(status)
    class(nml_snow1_t), intent(in) :: this
    character(len=*), intent(out), optional :: errmsg
    integer :: istat

    status = NML_OK
    if (present(errmsg)) errmsg = ""

    ! required arrays
    if (all(ieee_is_nan(this%snowTreshholdTemperature))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: snowTreshholdTemperature"
      return
    end if
    if (any(ieee_is_nan(this%snowTreshholdTemperature))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: snowTreshholdTemperature"
      return
    end if
    if (all(ieee_is_nan(this%degreeDayFactor_forest))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: degreeDayFactor_forest"
      return
    end if
    if (any(ieee_is_nan(this%degreeDayFactor_forest))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: degreeDayFactor_forest"
      return
    end if
    if (all(ieee_is_nan(this%degreeDayFactor_impervious))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: degreeDayFactor_impervious"
      return
    end if
    if (any(ieee_is_nan(this%degreeDayFactor_impervious))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: degreeDayFactor_impervious"
      return
    end if
    if (all(ieee_is_nan(this%degreeDayFactor_pervious))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: degreeDayFactor_pervious"
      return
    end if
    if (any(ieee_is_nan(this%degreeDayFactor_pervious))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: degreeDayFactor_pervious"
      return
    end if
    if (all(ieee_is_nan(this%increaseDegreeDayFactorByPrecip))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: increaseDegreeDayFactorByPrecip"
      return
    end if
    if (any(ieee_is_nan(this%increaseDegreeDayFactorByPrecip))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: increaseDegreeDayFactorByPrecip"
      return
    end if
    if (all(ieee_is_nan(this%maxDegreeDayFactor_forest))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: maxDegreeDayFactor_forest"
      return
    end if
    if (any(ieee_is_nan(this%maxDegreeDayFactor_forest))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: maxDegreeDayFactor_forest"
      return
    end if
    if (all(ieee_is_nan(this%maxDegreeDayFactor_impervious))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: maxDegreeDayFactor_impervious"
      return
    end if
    if (any(ieee_is_nan(this%maxDegreeDayFactor_impervious))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: maxDegreeDayFactor_impervious"
      return
    end if
    if (all(ieee_is_nan(this%maxDegreeDayFactor_pervious))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: maxDegreeDayFactor_pervious"
      return
    end if
    if (any(ieee_is_nan(this%maxDegreeDayFactor_pervious))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: maxDegreeDayFactor_pervious"
      return
    end if
  end function nml_snow1_is_valid

end module nml_snow1
