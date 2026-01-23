!> \file nml_parameter_pet2.f90
!> \copydoc nml_pet2

!> \brief PET - Case 2
!> \details Parameters for PET (case 2 - Priestley-Taylor).
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Jan 2026
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_namelists
module nml_pet2
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

  !> \class nml_pet2_t
  !> \brief PET - Case 2
  !> \details Parameters for PET (case 2 - Priestley-Taylor).
  type, public :: nml_pet2_t
    logical :: is_configured = .false. !< whether the namelist has been configured
    real(dp), dimension(5) :: PriestleyTaylorCoeff !< Priestley-Taylor coefficient
    real(dp), dimension(5) :: PriestleyTaylorLAIcorr !< Priestley-Taylor LAI correction factor
  contains
    procedure :: init => nml_pet2_init
    procedure :: from_file => nml_pet2_from_file
    procedure :: set => nml_pet2_set
    procedure :: is_set => nml_pet2_is_set
    procedure :: is_valid => nml_pet2_is_valid
  end type nml_pet2_t

contains

  !> \brief Initialize defaults and sentinels for pet2
  integer function nml_pet2_init(this, errmsg) result(status)
    class(nml_pet2_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    this%is_configured = .false.

    ! sentinel values for required/optional parameters
    this%PriestleyTaylorCoeff = ieee_value(this%PriestleyTaylorCoeff, ieee_quiet_nan) ! sentinel for required real array
    this%PriestleyTaylorLAIcorr = ieee_value(this%PriestleyTaylorLAIcorr, ieee_quiet_nan) ! sentinel for required real array
  end function nml_pet2_init

  !> \brief Read pet2 namelist from file
  integer function nml_pet2_from_file(this, file, errmsg) result(status)
    class(nml_pet2_t), intent(inout) :: this
    character(len=*), intent(in) :: file !< path to namelist file
    character(len=*), intent(out), optional :: errmsg
    ! namelist variables
    real(dp), dimension(5) :: PriestleyTaylorCoeff
    real(dp), dimension(5) :: PriestleyTaylorLAIcorr
    ! locals
    type(nml_file_t) :: nml
    integer :: iostat
    integer :: close_status
    character(len=nml_line_buffer) :: iomsg

    namelist /pet2/ &
      PriestleyTaylorCoeff, &
      PriestleyTaylorLAIcorr

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return
    PriestleyTaylorCoeff = this%PriestleyTaylorCoeff
    PriestleyTaylorLAIcorr = this%PriestleyTaylorLAIcorr

    status = nml%open(file, errmsg=errmsg)
    if (status /= NML_OK) return

    status = nml%find("pet2", errmsg=errmsg)
    if (status /= NML_OK) then
      close_status = nml%close()
      return
    end if

    ! read namelist
    read(nml%unit, nml=pet2, iostat=iostat, iomsg=iomsg)
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
    this%PriestleyTaylorCoeff = PriestleyTaylorCoeff
    this%PriestleyTaylorLAIcorr = PriestleyTaylorLAIcorr

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_pet2_from_file

  !> \brief Set pet2 values
  integer function nml_pet2_set(this, &
    PriestleyTaylorCoeff, &
    PriestleyTaylorLAIcorr, &
    errmsg) result(status)

    class(nml_pet2_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg
    real(dp), dimension(5), intent(in) :: PriestleyTaylorCoeff
    real(dp), dimension(5), intent(in) :: PriestleyTaylorLAIcorr

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return

    ! required parameters
    this%PriestleyTaylorCoeff = PriestleyTaylorCoeff
    this%PriestleyTaylorLAIcorr = PriestleyTaylorLAIcorr

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_pet2_set

  !> \brief Check whether a namelist value was set
  integer function nml_pet2_is_set(this, name, idx, errmsg) result(status)
    class(nml_pet2_t), intent(in) :: this
    character(len=*), intent(in) :: name
    integer, intent(in), optional :: idx(:)
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    select case (trim(name))
    case ("PriestleyTaylorCoeff")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%PriestleyTaylorCoeff), ubound(this%PriestleyTaylorCoeff), &
          "PriestleyTaylorCoeff", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%PriestleyTaylorCoeff(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%PriestleyTaylorCoeff))) status = NML_ERR_NOT_SET
      end if
    case ("PriestleyTaylorLAIcorr")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%PriestleyTaylorLAIcorr), ubound(this%PriestleyTaylorLAIcorr), &
          "PriestleyTaylorLAIcorr", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%PriestleyTaylorLAIcorr(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%PriestleyTaylorLAIcorr))) status = NML_ERR_NOT_SET
      end if
    case default
      status = NML_ERR_INVALID_NAME
      if (present(errmsg)) errmsg = "unknown field: " // trim(name)
    end select
    if (status == NML_ERR_NOT_SET .and. present(errmsg)) then
      if (len_trim(errmsg) == 0) errmsg = "field not set: " // trim(name)
    end if
  end function nml_pet2_is_set

  !> \brief Validate required values and constraints
  integer function nml_pet2_is_valid(this, errmsg) result(status)
    class(nml_pet2_t), intent(in) :: this
    character(len=*), intent(out), optional :: errmsg
    integer :: istat

    status = NML_OK
    if (present(errmsg)) errmsg = ""

    ! required arrays
    if (all(ieee_is_nan(this%PriestleyTaylorCoeff))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: PriestleyTaylorCoeff"
      return
    end if
    if (any(ieee_is_nan(this%PriestleyTaylorCoeff))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: PriestleyTaylorCoeff"
      return
    end if
    if (all(ieee_is_nan(this%PriestleyTaylorLAIcorr))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: PriestleyTaylorLAIcorr"
      return
    end if
    if (any(ieee_is_nan(this%PriestleyTaylorLAIcorr))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: PriestleyTaylorLAIcorr"
      return
    end if
  end function nml_pet2_is_valid

end module nml_pet2
