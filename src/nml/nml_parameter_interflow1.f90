!> \file nml_parameter_interflow1.f90
!> \copydoc nml_interflow1

!> \brief Interflow - Case 1
!> \details Parameters for interflow1.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Jan 2026
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_namelists
module nml_interflow1
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
    to_lower, &
    NML_ERR_PARTLY_SET
  use ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_is_nan
  ! kind specifiers listed in the nml-tools configuration file
  use mo_kind, only: &
    dp

  implicit none

  !> \class nml_interflow1_t
  !> \brief Interflow - Case 1
  !> \details Parameters for interflow1.
  type, public :: nml_interflow1_t
    logical :: is_configured = .false. !< whether the namelist has been configured
    real(dp), dimension(5) :: interflowstoragecapacityfactor !< Storage capacity factor for interflow
    real(dp), dimension(5) :: interflowrecession_slope !< Multiplier for slope to derive interflow recession constant
    real(dp), dimension(5) :: fastinterflowrecession_forest !< Multiplier for forest to derive fast interflow recession constant
    real(dp), dimension(5) :: slowinterflowrecession_ks !< Multiplier for variability of saturated hydraulic conductivity to derive slow interflow recession constant
    real(dp), dimension(5) :: exponentslowinterflow !< Multiplier for variability of saturated hydraulic conductivity to derive slow interflow exponent
  contains
    procedure :: init => nml_interflow1_init
    procedure :: from_file => nml_interflow1_from_file
    procedure :: set => nml_interflow1_set
    procedure :: is_set => nml_interflow1_is_set
    procedure :: is_valid => nml_interflow1_is_valid
  end type nml_interflow1_t

contains

  !> \brief Initialize defaults and sentinels for interflow1
  integer function nml_interflow1_init(this, errmsg) result(status)
    class(nml_interflow1_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    this%is_configured = .false.

    ! sentinel values for required/optional parameters
    this%interflowstoragecapacityfactor = ieee_value(this%interflowstoragecapacityfactor, ieee_quiet_nan) ! sentinel for required real array
    this%interflowrecession_slope = ieee_value(this%interflowrecession_slope, ieee_quiet_nan) ! sentinel for required real array
    this%fastinterflowrecession_forest = ieee_value(this%fastinterflowrecession_forest, ieee_quiet_nan) ! sentinel for required real array
    this%slowinterflowrecession_ks = ieee_value(this%slowinterflowrecession_ks, ieee_quiet_nan) ! sentinel for required real array
    this%exponentslowinterflow = ieee_value(this%exponentslowinterflow, ieee_quiet_nan) ! sentinel for required real array
  end function nml_interflow1_init

  !> \brief Read interflow1 namelist from file
  integer function nml_interflow1_from_file(this, file, errmsg) result(status)
    class(nml_interflow1_t), intent(inout) :: this
    character(len=*), intent(in) :: file !< path to namelist file
    character(len=*), intent(out), optional :: errmsg
    ! namelist variables
    real(dp), dimension(5) :: interflowstoragecapacityfactor
    real(dp), dimension(5) :: interflowrecession_slope
    real(dp), dimension(5) :: fastinterflowrecession_forest
    real(dp), dimension(5) :: slowinterflowrecession_ks
    real(dp), dimension(5) :: exponentslowinterflow
    ! locals
    type(nml_file_t) :: nml
    integer :: iostat
    integer :: close_status
    character(len=nml_line_buffer) :: iomsg

    namelist /interflow1/ &
      interflowstoragecapacityfactor, &
      interflowrecession_slope, &
      fastinterflowrecession_forest, &
      slowinterflowrecession_ks, &
      exponentslowinterflow

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return
    interflowstoragecapacityfactor = this%interflowstoragecapacityfactor
    interflowrecession_slope = this%interflowrecession_slope
    fastinterflowrecession_forest = this%fastinterflowrecession_forest
    slowinterflowrecession_ks = this%slowinterflowrecession_ks
    exponentslowinterflow = this%exponentslowinterflow

    status = nml%open(file, errmsg=errmsg)
    if (status /= NML_OK) return

    status = nml%find("interflow1", errmsg=errmsg)
    if (status /= NML_OK) then
      close_status = nml%close()
      return
    end if

    ! read namelist
    read(nml%unit, nml=interflow1, iostat=iostat, iomsg=iomsg)
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
    this%interflowstoragecapacityfactor = interflowstoragecapacityfactor
    this%interflowrecession_slope = interflowrecession_slope
    this%fastinterflowrecession_forest = fastinterflowrecession_forest
    this%slowinterflowrecession_ks = slowinterflowrecession_ks
    this%exponentslowinterflow = exponentslowinterflow

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_interflow1_from_file

  !> \brief Set interflow1 values
  integer function nml_interflow1_set(this, &
    interflowstoragecapacityfactor, &
    interflowrecession_slope, &
    fastinterflowrecession_forest, &
    slowinterflowrecession_ks, &
    exponentslowinterflow, &
    errmsg) result(status)

    class(nml_interflow1_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg
    real(dp), dimension(5), intent(in) :: interflowstoragecapacityfactor
    real(dp), dimension(5), intent(in) :: interflowrecession_slope
    real(dp), dimension(5), intent(in) :: fastinterflowrecession_forest
    real(dp), dimension(5), intent(in) :: slowinterflowrecession_ks
    real(dp), dimension(5), intent(in) :: exponentslowinterflow

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return

    ! required parameters
    this%interflowstoragecapacityfactor = interflowstoragecapacityfactor
    this%interflowrecession_slope = interflowrecession_slope
    this%fastinterflowrecession_forest = fastinterflowrecession_forest
    this%slowinterflowrecession_ks = slowinterflowrecession_ks
    this%exponentslowinterflow = exponentslowinterflow

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_interflow1_set

  !> \brief Check whether a namelist value was set
  integer function nml_interflow1_is_set(this, name, idx, errmsg) result(status)
    class(nml_interflow1_t), intent(in) :: this
    character(len=*), intent(in) :: name
    integer, intent(in), optional :: idx(:)
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    select case (to_lower(trim(name)))
    case ("interflowstoragecapacityfactor")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%interflowstoragecapacityfactor), ubound(this%interflowstoragecapacityfactor), &
          "interflowStorageCapacityFactor", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%interflowstoragecapacityfactor(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%interflowstoragecapacityfactor))) status = NML_ERR_NOT_SET
      end if
    case ("interflowrecession_slope")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%interflowrecession_slope), ubound(this%interflowrecession_slope), &
          "interflowRecession_slope", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%interflowrecession_slope(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%interflowrecession_slope))) status = NML_ERR_NOT_SET
      end if
    case ("fastinterflowrecession_forest")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%fastinterflowrecession_forest), ubound(this%fastinterflowrecession_forest), &
          "fastInterflowRecession_forest", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%fastinterflowrecession_forest(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%fastinterflowrecession_forest))) status = NML_ERR_NOT_SET
      end if
    case ("slowinterflowrecession_ks")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%slowinterflowrecession_ks), ubound(this%slowinterflowrecession_ks), &
          "slowInterflowRecession_Ks", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%slowinterflowrecession_ks(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%slowinterflowrecession_ks))) status = NML_ERR_NOT_SET
      end if
    case ("exponentslowinterflow")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%exponentslowinterflow), ubound(this%exponentslowinterflow), &
          "exponentSlowInterflow", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%exponentslowinterflow(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%exponentslowinterflow))) status = NML_ERR_NOT_SET
      end if
    case default
      status = NML_ERR_INVALID_NAME
      if (present(errmsg)) errmsg = "unknown field: " // trim(name)
    end select
    if (status == NML_ERR_NOT_SET .and. present(errmsg)) then
      if (len_trim(errmsg) == 0) errmsg = "field not set: " // trim(name)
    end if
  end function nml_interflow1_is_set

  !> \brief Validate required values and constraints
  integer function nml_interflow1_is_valid(this, errmsg) result(status)
    class(nml_interflow1_t), intent(in) :: this
    character(len=*), intent(out), optional :: errmsg
    integer :: istat

    status = NML_OK
    if (present(errmsg)) errmsg = ""

    ! required arrays
    if (all(ieee_is_nan(this%interflowstoragecapacityfactor))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: interflowStorageCapacityFactor"
      return
    end if
    if (any(ieee_is_nan(this%interflowstoragecapacityfactor))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: interflowStorageCapacityFactor"
      return
    end if
    if (all(ieee_is_nan(this%interflowrecession_slope))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: interflowRecession_slope"
      return
    end if
    if (any(ieee_is_nan(this%interflowrecession_slope))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: interflowRecession_slope"
      return
    end if
    if (all(ieee_is_nan(this%fastinterflowrecession_forest))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: fastInterflowRecession_forest"
      return
    end if
    if (any(ieee_is_nan(this%fastinterflowrecession_forest))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: fastInterflowRecession_forest"
      return
    end if
    if (all(ieee_is_nan(this%slowinterflowrecession_ks))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: slowInterflowRecession_Ks"
      return
    end if
    if (any(ieee_is_nan(this%slowinterflowrecession_ks))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: slowInterflowRecession_Ks"
      return
    end if
    if (all(ieee_is_nan(this%exponentslowinterflow))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: exponentSlowInterflow"
      return
    end if
    if (any(ieee_is_nan(this%exponentslowinterflow))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: exponentSlowInterflow"
      return
    end if
  end function nml_interflow1_is_valid

end module nml_interflow1
