!> \file nml_parameter_routing1.f90
!> \copydoc nml_routing1

!> \brief Routing - Case 1
!> \details Parameters for routing (case 1 - Muskingum).
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Jan 2026
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_namelists
module nml_routing1
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

  !> \class nml_routing1_t
  !> \brief Routing - Case 1
  !> \details Parameters for routing (case 1 - Muskingum).
  type, public :: nml_routing1_t
    logical :: is_configured = .false. !< whether the namelist has been configured
    real(dp), dimension(5) :: muskingumTravelTime_constant !< Muskingum travel time constant
    real(dp), dimension(5) :: muskingumTravelTime_riverLength !< Muskingum travel time river length
    real(dp), dimension(5) :: muskingumTravelTime_riverSlope !< Muskingum travel time river slope
    real(dp), dimension(5) :: muskingumTravelTime_impervious !< Muskingum travel time impervious
    real(dp), dimension(5) :: muskingumAttenuation_riverSlope !< Muskingum attenuation river slope
  contains
    procedure :: init => nml_routing1_init
    procedure :: from_file => nml_routing1_from_file
    procedure :: set => nml_routing1_set
    procedure :: is_set => nml_routing1_is_set
    procedure :: is_valid => nml_routing1_is_valid
  end type nml_routing1_t

contains

  !> \brief Initialize defaults and sentinels for routing1
  integer function nml_routing1_init(this, errmsg) result(status)
    class(nml_routing1_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    this%is_configured = .false.

    ! sentinel values for required/optional parameters
    this%muskingumTravelTime_constant = ieee_value(this%muskingumTravelTime_constant, ieee_quiet_nan) ! sentinel for required real array
    this%muskingumTravelTime_riverLength = ieee_value(this%muskingumTravelTime_riverLength, ieee_quiet_nan) ! sentinel for required real array
    this%muskingumTravelTime_riverSlope = ieee_value(this%muskingumTravelTime_riverSlope, ieee_quiet_nan) ! sentinel for required real array
    this%muskingumTravelTime_impervious = ieee_value(this%muskingumTravelTime_impervious, ieee_quiet_nan) ! sentinel for required real array
    this%muskingumAttenuation_riverSlope = ieee_value(this%muskingumAttenuation_riverSlope, ieee_quiet_nan) ! sentinel for required real array
  end function nml_routing1_init

  !> \brief Read routing1 namelist from file
  integer function nml_routing1_from_file(this, file, errmsg) result(status)
    class(nml_routing1_t), intent(inout) :: this
    character(len=*), intent(in) :: file !< path to namelist file
    character(len=*), intent(out), optional :: errmsg
    ! namelist variables
    real(dp), dimension(5) :: muskingumTravelTime_constant
    real(dp), dimension(5) :: muskingumTravelTime_riverLength
    real(dp), dimension(5) :: muskingumTravelTime_riverSlope
    real(dp), dimension(5) :: muskingumTravelTime_impervious
    real(dp), dimension(5) :: muskingumAttenuation_riverSlope
    ! locals
    type(nml_file_t) :: nml
    integer :: iostat
    integer :: close_status
    character(len=nml_line_buffer) :: iomsg

    namelist /routing1/ &
      muskingumTravelTime_constant, &
      muskingumTravelTime_riverLength, &
      muskingumTravelTime_riverSlope, &
      muskingumTravelTime_impervious, &
      muskingumAttenuation_riverSlope

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return
    muskingumTravelTime_constant = this%muskingumTravelTime_constant
    muskingumTravelTime_riverLength = this%muskingumTravelTime_riverLength
    muskingumTravelTime_riverSlope = this%muskingumTravelTime_riverSlope
    muskingumTravelTime_impervious = this%muskingumTravelTime_impervious
    muskingumAttenuation_riverSlope = this%muskingumAttenuation_riverSlope

    status = nml%open(file, errmsg=errmsg)
    if (status /= NML_OK) return

    status = nml%find("routing1", errmsg=errmsg)
    if (status /= NML_OK) then
      close_status = nml%close()
      return
    end if

    ! read namelist
    read(nml%unit, nml=routing1, iostat=iostat, iomsg=iomsg)
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
    this%muskingumTravelTime_constant = muskingumTravelTime_constant
    this%muskingumTravelTime_riverLength = muskingumTravelTime_riverLength
    this%muskingumTravelTime_riverSlope = muskingumTravelTime_riverSlope
    this%muskingumTravelTime_impervious = muskingumTravelTime_impervious
    this%muskingumAttenuation_riverSlope = muskingumAttenuation_riverSlope

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_routing1_from_file

  !> \brief Set routing1 values
  integer function nml_routing1_set(this, &
    muskingumTravelTime_constant, &
    muskingumTravelTime_riverLength, &
    muskingumTravelTime_riverSlope, &
    muskingumTravelTime_impervious, &
    muskingumAttenuation_riverSlope, &
    errmsg) result(status)

    class(nml_routing1_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg
    real(dp), dimension(5), intent(in) :: muskingumTravelTime_constant
    real(dp), dimension(5), intent(in) :: muskingumTravelTime_riverLength
    real(dp), dimension(5), intent(in) :: muskingumTravelTime_riverSlope
    real(dp), dimension(5), intent(in) :: muskingumTravelTime_impervious
    real(dp), dimension(5), intent(in) :: muskingumAttenuation_riverSlope

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return

    ! required parameters
    this%muskingumTravelTime_constant = muskingumTravelTime_constant
    this%muskingumTravelTime_riverLength = muskingumTravelTime_riverLength
    this%muskingumTravelTime_riverSlope = muskingumTravelTime_riverSlope
    this%muskingumTravelTime_impervious = muskingumTravelTime_impervious
    this%muskingumAttenuation_riverSlope = muskingumAttenuation_riverSlope

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_routing1_set

  !> \brief Check whether a namelist value was set
  integer function nml_routing1_is_set(this, name, idx, errmsg) result(status)
    class(nml_routing1_t), intent(in) :: this
    character(len=*), intent(in) :: name
    integer, intent(in), optional :: idx(:)
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    select case (trim(name))
    case ("muskingumTravelTime_constant")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%muskingumTravelTime_constant), ubound(this%muskingumTravelTime_constant), &
          "muskingumTravelTime_constant", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%muskingumTravelTime_constant(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%muskingumTravelTime_constant))) status = NML_ERR_NOT_SET
      end if
    case ("muskingumTravelTime_riverLength")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%muskingumTravelTime_riverLength), ubound(this%muskingumTravelTime_riverLength), &
          "muskingumTravelTime_riverLength", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%muskingumTravelTime_riverLength(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%muskingumTravelTime_riverLength))) status = NML_ERR_NOT_SET
      end if
    case ("muskingumTravelTime_riverSlope")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%muskingumTravelTime_riverSlope), ubound(this%muskingumTravelTime_riverSlope), &
          "muskingumTravelTime_riverSlope", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%muskingumTravelTime_riverSlope(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%muskingumTravelTime_riverSlope))) status = NML_ERR_NOT_SET
      end if
    case ("muskingumTravelTime_impervious")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%muskingumTravelTime_impervious), ubound(this%muskingumTravelTime_impervious), &
          "muskingumTravelTime_impervious", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%muskingumTravelTime_impervious(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%muskingumTravelTime_impervious))) status = NML_ERR_NOT_SET
      end if
    case ("muskingumAttenuation_riverSlope")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%muskingumAttenuation_riverSlope), ubound(this%muskingumAttenuation_riverSlope), &
          "muskingumAttenuation_riverSlope", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%muskingumAttenuation_riverSlope(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%muskingumAttenuation_riverSlope))) status = NML_ERR_NOT_SET
      end if
    case default
      status = NML_ERR_INVALID_NAME
      if (present(errmsg)) errmsg = "unknown field: " // trim(name)
    end select
    if (status == NML_ERR_NOT_SET .and. present(errmsg)) then
      if (len_trim(errmsg) == 0) errmsg = "field not set: " // trim(name)
    end if
  end function nml_routing1_is_set

  !> \brief Validate required values and constraints
  integer function nml_routing1_is_valid(this, errmsg) result(status)
    class(nml_routing1_t), intent(in) :: this
    character(len=*), intent(out), optional :: errmsg
    integer :: istat

    status = NML_OK
    if (present(errmsg)) errmsg = ""

    ! required arrays
    if (all(ieee_is_nan(this%muskingumTravelTime_constant))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: muskingumTravelTime_constant"
      return
    end if
    if (any(ieee_is_nan(this%muskingumTravelTime_constant))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: muskingumTravelTime_constant"
      return
    end if
    if (all(ieee_is_nan(this%muskingumTravelTime_riverLength))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: muskingumTravelTime_riverLength"
      return
    end if
    if (any(ieee_is_nan(this%muskingumTravelTime_riverLength))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: muskingumTravelTime_riverLength"
      return
    end if
    if (all(ieee_is_nan(this%muskingumTravelTime_riverSlope))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: muskingumTravelTime_riverSlope"
      return
    end if
    if (any(ieee_is_nan(this%muskingumTravelTime_riverSlope))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: muskingumTravelTime_riverSlope"
      return
    end if
    if (all(ieee_is_nan(this%muskingumTravelTime_impervious))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: muskingumTravelTime_impervious"
      return
    end if
    if (any(ieee_is_nan(this%muskingumTravelTime_impervious))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: muskingumTravelTime_impervious"
      return
    end if
    if (all(ieee_is_nan(this%muskingumAttenuation_riverSlope))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: muskingumAttenuation_riverSlope"
      return
    end if
    if (any(ieee_is_nan(this%muskingumAttenuation_riverSlope))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: muskingumAttenuation_riverSlope"
      return
    end if
  end function nml_routing1_is_valid

end module nml_routing1
