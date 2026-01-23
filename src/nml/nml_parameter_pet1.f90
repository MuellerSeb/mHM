!> \file nml_parameter_pet1.f90
!> \copydoc nml_pet1

!> \brief PET - Case 1
!> \details Parameters for PET (case 1 - Hargreaves-Samani).
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Jan 2026
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_namelists
module nml_pet1
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

  !> \class nml_pet1_t
  !> \brief PET - Case 1
  !> \details Parameters for PET (case 1 - Hargreaves-Samani).
  type, public :: nml_pet1_t
    logical :: is_configured = .false. !< whether the namelist has been configured
    real(dp), dimension(5) :: minCorrectionFactorPET !< minimum correction factor for PET
    real(dp), dimension(5) :: maxCorrectionFactorPET !< maximum correction factor for PET
    real(dp), dimension(5) :: aspectTresholdPET !< aspect threshold for PET
    real(dp), dimension(5) :: HargreavesSamaniCoeff !< Hargreaves-Samani coefficient
  contains
    procedure :: init => nml_pet1_init
    procedure :: from_file => nml_pet1_from_file
    procedure :: set => nml_pet1_set
    procedure :: is_set => nml_pet1_is_set
    procedure :: is_valid => nml_pet1_is_valid
  end type nml_pet1_t

contains

  !> \brief Initialize defaults and sentinels for pet1
  integer function nml_pet1_init(this, errmsg) result(status)
    class(nml_pet1_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    this%is_configured = .false.

    ! sentinel values for required/optional parameters
    this%minCorrectionFactorPET = ieee_value(this%minCorrectionFactorPET, ieee_quiet_nan) ! sentinel for required real array
    this%maxCorrectionFactorPET = ieee_value(this%maxCorrectionFactorPET, ieee_quiet_nan) ! sentinel for required real array
    this%aspectTresholdPET = ieee_value(this%aspectTresholdPET, ieee_quiet_nan) ! sentinel for required real array
    this%HargreavesSamaniCoeff = ieee_value(this%HargreavesSamaniCoeff, ieee_quiet_nan) ! sentinel for required real array
  end function nml_pet1_init

  !> \brief Read pet1 namelist from file
  integer function nml_pet1_from_file(this, file, errmsg) result(status)
    class(nml_pet1_t), intent(inout) :: this
    character(len=*), intent(in) :: file !< path to namelist file
    character(len=*), intent(out), optional :: errmsg
    ! namelist variables
    real(dp), dimension(5) :: minCorrectionFactorPET
    real(dp), dimension(5) :: maxCorrectionFactorPET
    real(dp), dimension(5) :: aspectTresholdPET
    real(dp), dimension(5) :: HargreavesSamaniCoeff
    ! locals
    type(nml_file_t) :: nml
    integer :: iostat
    integer :: close_status
    character(len=nml_line_buffer) :: iomsg

    namelist /pet1/ &
      minCorrectionFactorPET, &
      maxCorrectionFactorPET, &
      aspectTresholdPET, &
      HargreavesSamaniCoeff

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return
    minCorrectionFactorPET = this%minCorrectionFactorPET
    maxCorrectionFactorPET = this%maxCorrectionFactorPET
    aspectTresholdPET = this%aspectTresholdPET
    HargreavesSamaniCoeff = this%HargreavesSamaniCoeff

    status = nml%open(file, errmsg=errmsg)
    if (status /= NML_OK) return

    status = nml%find("pet1", errmsg=errmsg)
    if (status /= NML_OK) then
      close_status = nml%close()
      return
    end if

    ! read namelist
    read(nml%unit, nml=pet1, iostat=iostat, iomsg=iomsg)
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
    this%minCorrectionFactorPET = minCorrectionFactorPET
    this%maxCorrectionFactorPET = maxCorrectionFactorPET
    this%aspectTresholdPET = aspectTresholdPET
    this%HargreavesSamaniCoeff = HargreavesSamaniCoeff

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_pet1_from_file

  !> \brief Set pet1 values
  integer function nml_pet1_set(this, &
    minCorrectionFactorPET, &
    maxCorrectionFactorPET, &
    aspectTresholdPET, &
    HargreavesSamaniCoeff, &
    errmsg) result(status)

    class(nml_pet1_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg
    real(dp), dimension(5), intent(in) :: minCorrectionFactorPET
    real(dp), dimension(5), intent(in) :: maxCorrectionFactorPET
    real(dp), dimension(5), intent(in) :: aspectTresholdPET
    real(dp), dimension(5), intent(in) :: HargreavesSamaniCoeff

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return

    ! required parameters
    this%minCorrectionFactorPET = minCorrectionFactorPET
    this%maxCorrectionFactorPET = maxCorrectionFactorPET
    this%aspectTresholdPET = aspectTresholdPET
    this%HargreavesSamaniCoeff = HargreavesSamaniCoeff

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_pet1_set

  !> \brief Check whether a namelist value was set
  integer function nml_pet1_is_set(this, name, idx, errmsg) result(status)
    class(nml_pet1_t), intent(in) :: this
    character(len=*), intent(in) :: name
    integer, intent(in), optional :: idx(:)
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    select case (trim(name))
    case ("minCorrectionFactorPET")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%minCorrectionFactorPET), ubound(this%minCorrectionFactorPET), &
          "minCorrectionFactorPET", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%minCorrectionFactorPET(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%minCorrectionFactorPET))) status = NML_ERR_NOT_SET
      end if
    case ("maxCorrectionFactorPET")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%maxCorrectionFactorPET), ubound(this%maxCorrectionFactorPET), &
          "maxCorrectionFactorPET", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%maxCorrectionFactorPET(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%maxCorrectionFactorPET))) status = NML_ERR_NOT_SET
      end if
    case ("aspectTresholdPET")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%aspectTresholdPET), ubound(this%aspectTresholdPET), &
          "aspectTresholdPET", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%aspectTresholdPET(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%aspectTresholdPET))) status = NML_ERR_NOT_SET
      end if
    case ("HargreavesSamaniCoeff")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%HargreavesSamaniCoeff), ubound(this%HargreavesSamaniCoeff), &
          "HargreavesSamaniCoeff", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%HargreavesSamaniCoeff(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%HargreavesSamaniCoeff))) status = NML_ERR_NOT_SET
      end if
    case default
      status = NML_ERR_INVALID_NAME
      if (present(errmsg)) errmsg = "unknown field: " // trim(name)
    end select
    if (status == NML_ERR_NOT_SET .and. present(errmsg)) then
      if (len_trim(errmsg) == 0) errmsg = "field not set: " // trim(name)
    end if
  end function nml_pet1_is_set

  !> \brief Validate required values and constraints
  integer function nml_pet1_is_valid(this, errmsg) result(status)
    class(nml_pet1_t), intent(in) :: this
    character(len=*), intent(out), optional :: errmsg
    integer :: istat

    status = NML_OK
    if (present(errmsg)) errmsg = ""

    ! required arrays
    if (all(ieee_is_nan(this%minCorrectionFactorPET))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: minCorrectionFactorPET"
      return
    end if
    if (any(ieee_is_nan(this%minCorrectionFactorPET))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: minCorrectionFactorPET"
      return
    end if
    if (all(ieee_is_nan(this%maxCorrectionFactorPET))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: maxCorrectionFactorPET"
      return
    end if
    if (any(ieee_is_nan(this%maxCorrectionFactorPET))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: maxCorrectionFactorPET"
      return
    end if
    if (all(ieee_is_nan(this%aspectTresholdPET))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: aspectTresholdPET"
      return
    end if
    if (any(ieee_is_nan(this%aspectTresholdPET))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: aspectTresholdPET"
      return
    end if
    if (all(ieee_is_nan(this%HargreavesSamaniCoeff))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: HargreavesSamaniCoeff"
      return
    end if
    if (any(ieee_is_nan(this%HargreavesSamaniCoeff))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: HargreavesSamaniCoeff"
      return
    end if
  end function nml_pet1_is_valid

end module nml_pet1
