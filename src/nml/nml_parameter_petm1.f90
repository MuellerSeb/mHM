!> \file nml_parameter_petm1.f90
!> \copydoc nml_petm1

!> \brief PET - Case -1
!> \details Parameters for PET (case -1 - LAI correction).
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Jan 2026
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_namelists
module nml_petm1
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

  !> \class nml_petm1_t
  !> \brief PET - Case -1
  !> \details Parameters for PET (case -1 - LAI correction).
  type, public :: nml_petm1_t
    logical :: is_configured = .false. !< whether the namelist has been configured
    real(dp), dimension(5) :: PET_a_forest !< Potential evapotranspiration forest
    real(dp), dimension(5) :: PET_a_impervious !< Potential evapotranspiration impervious
    real(dp), dimension(5) :: PET_a_pervious !< Potential evapotranspiration pervious
    real(dp), dimension(5) :: PET_b !< Potential evapotranspiration b
    real(dp), dimension(5) :: PET_c !< Potential evapotranspiration c
  contains
    procedure :: init => nml_petm1_init
    procedure :: from_file => nml_petm1_from_file
    procedure :: set => nml_petm1_set
    procedure :: is_set => nml_petm1_is_set
    procedure :: is_valid => nml_petm1_is_valid
  end type nml_petm1_t

contains

  !> \brief Initialize defaults and sentinels for petm1
  integer function nml_petm1_init(this, errmsg) result(status)
    class(nml_petm1_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    this%is_configured = .false.

    ! sentinel values for required/optional parameters
    this%PET_a_forest = ieee_value(this%PET_a_forest, ieee_quiet_nan) ! sentinel for required real array
    this%PET_a_impervious = ieee_value(this%PET_a_impervious, ieee_quiet_nan) ! sentinel for required real array
    this%PET_a_pervious = ieee_value(this%PET_a_pervious, ieee_quiet_nan) ! sentinel for required real array
    this%PET_b = ieee_value(this%PET_b, ieee_quiet_nan) ! sentinel for required real array
    this%PET_c = ieee_value(this%PET_c, ieee_quiet_nan) ! sentinel for required real array
  end function nml_petm1_init

  !> \brief Read petm1 namelist from file
  integer function nml_petm1_from_file(this, file, errmsg) result(status)
    class(nml_petm1_t), intent(inout) :: this
    character(len=*), intent(in) :: file !< path to namelist file
    character(len=*), intent(out), optional :: errmsg
    ! namelist variables
    real(dp), dimension(5) :: PET_a_forest
    real(dp), dimension(5) :: PET_a_impervious
    real(dp), dimension(5) :: PET_a_pervious
    real(dp), dimension(5) :: PET_b
    real(dp), dimension(5) :: PET_c
    ! locals
    type(nml_file_t) :: nml
    integer :: iostat
    integer :: close_status
    character(len=nml_line_buffer) :: iomsg

    namelist /petm1/ &
      PET_a_forest, &
      PET_a_impervious, &
      PET_a_pervious, &
      PET_b, &
      PET_c

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return
    PET_a_forest = this%PET_a_forest
    PET_a_impervious = this%PET_a_impervious
    PET_a_pervious = this%PET_a_pervious
    PET_b = this%PET_b
    PET_c = this%PET_c

    status = nml%open(file, errmsg=errmsg)
    if (status /= NML_OK) return

    status = nml%find("petm1", errmsg=errmsg)
    if (status /= NML_OK) then
      close_status = nml%close()
      return
    end if

    ! read namelist
    read(nml%unit, nml=petm1, iostat=iostat, iomsg=iomsg)
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
    this%PET_a_forest = PET_a_forest
    this%PET_a_impervious = PET_a_impervious
    this%PET_a_pervious = PET_a_pervious
    this%PET_b = PET_b
    this%PET_c = PET_c

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_petm1_from_file

  !> \brief Set petm1 values
  integer function nml_petm1_set(this, &
    PET_a_forest, &
    PET_a_impervious, &
    PET_a_pervious, &
    PET_b, &
    PET_c, &
    errmsg) result(status)

    class(nml_petm1_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg
    real(dp), dimension(5), intent(in) :: PET_a_forest
    real(dp), dimension(5), intent(in) :: PET_a_impervious
    real(dp), dimension(5), intent(in) :: PET_a_pervious
    real(dp), dimension(5), intent(in) :: PET_b
    real(dp), dimension(5), intent(in) :: PET_c

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return

    ! required parameters
    this%PET_a_forest = PET_a_forest
    this%PET_a_impervious = PET_a_impervious
    this%PET_a_pervious = PET_a_pervious
    this%PET_b = PET_b
    this%PET_c = PET_c

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_petm1_set

  !> \brief Check whether a namelist value was set
  integer function nml_petm1_is_set(this, name, idx, errmsg) result(status)
    class(nml_petm1_t), intent(in) :: this
    character(len=*), intent(in) :: name
    integer, intent(in), optional :: idx(:)
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    select case (trim(name))
    case ("PET_a_forest")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%PET_a_forest), ubound(this%PET_a_forest), &
          "PET_a_forest", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%PET_a_forest(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%PET_a_forest))) status = NML_ERR_NOT_SET
      end if
    case ("PET_a_impervious")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%PET_a_impervious), ubound(this%PET_a_impervious), &
          "PET_a_impervious", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%PET_a_impervious(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%PET_a_impervious))) status = NML_ERR_NOT_SET
      end if
    case ("PET_a_pervious")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%PET_a_pervious), ubound(this%PET_a_pervious), &
          "PET_a_pervious", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%PET_a_pervious(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%PET_a_pervious))) status = NML_ERR_NOT_SET
      end if
    case ("PET_b")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%PET_b), ubound(this%PET_b), &
          "PET_b", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%PET_b(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%PET_b))) status = NML_ERR_NOT_SET
      end if
    case ("PET_c")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%PET_c), ubound(this%PET_c), &
          "PET_c", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%PET_c(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%PET_c))) status = NML_ERR_NOT_SET
      end if
    case default
      status = NML_ERR_INVALID_NAME
      if (present(errmsg)) errmsg = "unknown field: " // trim(name)
    end select
    if (status == NML_ERR_NOT_SET .and. present(errmsg)) then
      if (len_trim(errmsg) == 0) errmsg = "field not set: " // trim(name)
    end if
  end function nml_petm1_is_set

  !> \brief Validate required values and constraints
  integer function nml_petm1_is_valid(this, errmsg) result(status)
    class(nml_petm1_t), intent(in) :: this
    character(len=*), intent(out), optional :: errmsg
    integer :: istat

    status = NML_OK
    if (present(errmsg)) errmsg = ""

    ! required arrays
    if (all(ieee_is_nan(this%PET_a_forest))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: PET_a_forest"
      return
    end if
    if (any(ieee_is_nan(this%PET_a_forest))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: PET_a_forest"
      return
    end if
    if (all(ieee_is_nan(this%PET_a_impervious))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: PET_a_impervious"
      return
    end if
    if (any(ieee_is_nan(this%PET_a_impervious))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: PET_a_impervious"
      return
    end if
    if (all(ieee_is_nan(this%PET_a_pervious))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: PET_a_pervious"
      return
    end if
    if (any(ieee_is_nan(this%PET_a_pervious))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: PET_a_pervious"
      return
    end if
    if (all(ieee_is_nan(this%PET_b))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: PET_b"
      return
    end if
    if (any(ieee_is_nan(this%PET_b))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: PET_b"
      return
    end if
    if (all(ieee_is_nan(this%PET_c))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: PET_c"
      return
    end if
    if (any(ieee_is_nan(this%PET_c))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: PET_c"
      return
    end if
  end function nml_petm1_is_valid

end module nml_petm1
