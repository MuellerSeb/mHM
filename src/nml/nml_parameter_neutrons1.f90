!> \file nml_parameter_neutrons1.f90
!> \copydoc nml_neutrons1

!> \brief Neutrons - Case 1
!> \details Parameters for neutrons (case 1 - Desilets).
!! Ground albedo neutrons - DESILET version.
!! THIS IS WORK IN PROGRESS, DO NOT USE FOR RESEARCH
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Jan 2026
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_namelists
module nml_neutrons1
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

  !> \class nml_neutrons1_t
  !> \brief Neutrons - Case 1
  !> \details Parameters for neutrons (case 1 - Desilets).
  !! Ground albedo neutrons - DESILET version.
  !! THIS IS WORK IN PROGRESS, DO NOT USE FOR RESEARCH
  type, public :: nml_neutrons1_t
    logical :: is_configured = .false. !< whether the namelist has been configured
    real(dp), dimension(5) :: Desilets_N0 !< Desilets N0 dry count
    real(dp), dimension(5) :: Desilets_LW0 !< Desilets LW0 parameter
    real(dp), dimension(5) :: Desilets_LW1 !< Desilets LW1 parameter
  contains
    procedure :: init => nml_neutrons1_init
    procedure :: from_file => nml_neutrons1_from_file
    procedure :: set => nml_neutrons1_set
    procedure :: is_set => nml_neutrons1_is_set
    procedure :: is_valid => nml_neutrons1_is_valid
  end type nml_neutrons1_t

contains

  !> \brief Initialize defaults and sentinels for neutrons1
  integer function nml_neutrons1_init(this, errmsg) result(status)
    class(nml_neutrons1_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    this%is_configured = .false.

    ! sentinel values for required/optional parameters
    this%Desilets_N0 = ieee_value(this%Desilets_N0, ieee_quiet_nan) ! sentinel for required real array
    this%Desilets_LW0 = ieee_value(this%Desilets_LW0, ieee_quiet_nan) ! sentinel for required real array
    this%Desilets_LW1 = ieee_value(this%Desilets_LW1, ieee_quiet_nan) ! sentinel for required real array
  end function nml_neutrons1_init

  !> \brief Read neutrons1 namelist from file
  integer function nml_neutrons1_from_file(this, file, errmsg) result(status)
    class(nml_neutrons1_t), intent(inout) :: this
    character(len=*), intent(in) :: file !< path to namelist file
    character(len=*), intent(out), optional :: errmsg
    ! namelist variables
    real(dp), dimension(5) :: Desilets_N0
    real(dp), dimension(5) :: Desilets_LW0
    real(dp), dimension(5) :: Desilets_LW1
    ! locals
    type(nml_file_t) :: nml
    integer :: iostat
    integer :: close_status
    character(len=nml_line_buffer) :: iomsg

    namelist /neutrons1/ &
      Desilets_N0, &
      Desilets_LW0, &
      Desilets_LW1

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return
    Desilets_N0 = this%Desilets_N0
    Desilets_LW0 = this%Desilets_LW0
    Desilets_LW1 = this%Desilets_LW1

    status = nml%open(file, errmsg=errmsg)
    if (status /= NML_OK) return

    status = nml%find("neutrons1", errmsg=errmsg)
    if (status /= NML_OK) then
      close_status = nml%close()
      return
    end if

    ! read namelist
    read(nml%unit, nml=neutrons1, iostat=iostat, iomsg=iomsg)
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
    this%Desilets_N0 = Desilets_N0
    this%Desilets_LW0 = Desilets_LW0
    this%Desilets_LW1 = Desilets_LW1

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_neutrons1_from_file

  !> \brief Set neutrons1 values
  integer function nml_neutrons1_set(this, &
    Desilets_N0, &
    Desilets_LW0, &
    Desilets_LW1, &
    errmsg) result(status)

    class(nml_neutrons1_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg
    real(dp), dimension(5), intent(in) :: Desilets_N0
    real(dp), dimension(5), intent(in) :: Desilets_LW0
    real(dp), dimension(5), intent(in) :: Desilets_LW1

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return

    ! required parameters
    this%Desilets_N0 = Desilets_N0
    this%Desilets_LW0 = Desilets_LW0
    this%Desilets_LW1 = Desilets_LW1

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_neutrons1_set

  !> \brief Check whether a namelist value was set
  integer function nml_neutrons1_is_set(this, name, idx, errmsg) result(status)
    class(nml_neutrons1_t), intent(in) :: this
    character(len=*), intent(in) :: name
    integer, intent(in), optional :: idx(:)
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    select case (trim(name))
    case ("Desilets_N0")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%Desilets_N0), ubound(this%Desilets_N0), &
          "Desilets_N0", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%Desilets_N0(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%Desilets_N0))) status = NML_ERR_NOT_SET
      end if
    case ("Desilets_LW0")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%Desilets_LW0), ubound(this%Desilets_LW0), &
          "Desilets_LW0", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%Desilets_LW0(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%Desilets_LW0))) status = NML_ERR_NOT_SET
      end if
    case ("Desilets_LW1")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%Desilets_LW1), ubound(this%Desilets_LW1), &
          "Desilets_LW1", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%Desilets_LW1(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%Desilets_LW1))) status = NML_ERR_NOT_SET
      end if
    case default
      status = NML_ERR_INVALID_NAME
      if (present(errmsg)) errmsg = "unknown field: " // trim(name)
    end select
    if (status == NML_ERR_NOT_SET .and. present(errmsg)) then
      if (len_trim(errmsg) == 0) errmsg = "field not set: " // trim(name)
    end if
  end function nml_neutrons1_is_set

  !> \brief Validate required values and constraints
  integer function nml_neutrons1_is_valid(this, errmsg) result(status)
    class(nml_neutrons1_t), intent(in) :: this
    character(len=*), intent(out), optional :: errmsg
    integer :: istat

    status = NML_OK
    if (present(errmsg)) errmsg = ""

    ! required arrays
    if (all(ieee_is_nan(this%Desilets_N0))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: Desilets_N0"
      return
    end if
    if (any(ieee_is_nan(this%Desilets_N0))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: Desilets_N0"
      return
    end if
    if (all(ieee_is_nan(this%Desilets_LW0))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: Desilets_LW0"
      return
    end if
    if (any(ieee_is_nan(this%Desilets_LW0))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: Desilets_LW0"
      return
    end if
    if (all(ieee_is_nan(this%Desilets_LW1))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: Desilets_LW1"
      return
    end if
    if (any(ieee_is_nan(this%Desilets_LW1))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: Desilets_LW1"
      return
    end if
  end function nml_neutrons1_is_valid

end module nml_neutrons1
