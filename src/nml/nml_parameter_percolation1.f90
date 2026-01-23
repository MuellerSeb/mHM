!> \file nml_parameter_percolation1.f90
!> \copydoc nml_percolation1

!> \brief Percolation - Case 1
!> \details Parameters for percolation.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Jan 2026
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_namelists
module nml_percolation1
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

  !> \class nml_percolation1_t
  !> \brief Percolation - Case 1
  !> \details Parameters for percolation.
  type, public :: nml_percolation1_t
    logical :: is_configured = .false. !< whether the namelist has been configured
    real(dp), dimension(5) :: rechargeCoefficient !< Recharge coefficient
    real(dp), dimension(5) :: rechargeFactor_karstic !< Recharge factor karstic
    real(dp), dimension(5) :: gain_loss_GWreservoir_karstic !< Gain/loss GW reservoir karstic
  contains
    procedure :: init => nml_percolation1_init
    procedure :: from_file => nml_percolation1_from_file
    procedure :: set => nml_percolation1_set
    procedure :: is_set => nml_percolation1_is_set
    procedure :: is_valid => nml_percolation1_is_valid
  end type nml_percolation1_t

contains

  !> \brief Initialize defaults and sentinels for percolation1
  integer function nml_percolation1_init(this, errmsg) result(status)
    class(nml_percolation1_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    this%is_configured = .false.

    ! sentinel values for required/optional parameters
    this%rechargeCoefficient = ieee_value(this%rechargeCoefficient, ieee_quiet_nan) ! sentinel for required real array
    this%rechargeFactor_karstic = ieee_value(this%rechargeFactor_karstic, ieee_quiet_nan) ! sentinel for required real array
    this%gain_loss_GWreservoir_karstic = ieee_value(this%gain_loss_GWreservoir_karstic, ieee_quiet_nan) ! sentinel for required real array
  end function nml_percolation1_init

  !> \brief Read percolation1 namelist from file
  integer function nml_percolation1_from_file(this, file, errmsg) result(status)
    class(nml_percolation1_t), intent(inout) :: this
    character(len=*), intent(in) :: file !< path to namelist file
    character(len=*), intent(out), optional :: errmsg
    ! namelist variables
    real(dp), dimension(5) :: rechargeCoefficient
    real(dp), dimension(5) :: rechargeFactor_karstic
    real(dp), dimension(5) :: gain_loss_GWreservoir_karstic
    ! locals
    type(nml_file_t) :: nml
    integer :: iostat
    integer :: close_status
    character(len=nml_line_buffer) :: iomsg

    namelist /percolation1/ &
      rechargeCoefficient, &
      rechargeFactor_karstic, &
      gain_loss_GWreservoir_karstic

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return
    rechargeCoefficient = this%rechargeCoefficient
    rechargeFactor_karstic = this%rechargeFactor_karstic
    gain_loss_GWreservoir_karstic = this%gain_loss_GWreservoir_karstic

    status = nml%open(file, errmsg=errmsg)
    if (status /= NML_OK) return

    status = nml%find("percolation1", errmsg=errmsg)
    if (status /= NML_OK) then
      close_status = nml%close()
      return
    end if

    ! read namelist
    read(nml%unit, nml=percolation1, iostat=iostat, iomsg=iomsg)
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
    this%rechargeCoefficient = rechargeCoefficient
    this%rechargeFactor_karstic = rechargeFactor_karstic
    this%gain_loss_GWreservoir_karstic = gain_loss_GWreservoir_karstic

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_percolation1_from_file

  !> \brief Set percolation1 values
  integer function nml_percolation1_set(this, &
    rechargeCoefficient, &
    rechargeFactor_karstic, &
    gain_loss_GWreservoir_karstic, &
    errmsg) result(status)

    class(nml_percolation1_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg
    real(dp), dimension(5), intent(in) :: rechargeCoefficient
    real(dp), dimension(5), intent(in) :: rechargeFactor_karstic
    real(dp), dimension(5), intent(in) :: gain_loss_GWreservoir_karstic

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return

    ! required parameters
    this%rechargeCoefficient = rechargeCoefficient
    this%rechargeFactor_karstic = rechargeFactor_karstic
    this%gain_loss_GWreservoir_karstic = gain_loss_GWreservoir_karstic

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_percolation1_set

  !> \brief Check whether a namelist value was set
  integer function nml_percolation1_is_set(this, name, idx, errmsg) result(status)
    class(nml_percolation1_t), intent(in) :: this
    character(len=*), intent(in) :: name
    integer, intent(in), optional :: idx(:)
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    select case (trim(name))
    case ("rechargeCoefficient")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%rechargeCoefficient), ubound(this%rechargeCoefficient), &
          "rechargeCoefficient", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%rechargeCoefficient(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%rechargeCoefficient))) status = NML_ERR_NOT_SET
      end if
    case ("rechargeFactor_karstic")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%rechargeFactor_karstic), ubound(this%rechargeFactor_karstic), &
          "rechargeFactor_karstic", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%rechargeFactor_karstic(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%rechargeFactor_karstic))) status = NML_ERR_NOT_SET
      end if
    case ("gain_loss_GWreservoir_karstic")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%gain_loss_GWreservoir_karstic), ubound(this%gain_loss_GWreservoir_karstic), &
          "gain_loss_GWreservoir_karstic", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%gain_loss_GWreservoir_karstic(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%gain_loss_GWreservoir_karstic))) status = NML_ERR_NOT_SET
      end if
    case default
      status = NML_ERR_INVALID_NAME
      if (present(errmsg)) errmsg = "unknown field: " // trim(name)
    end select
    if (status == NML_ERR_NOT_SET .and. present(errmsg)) then
      if (len_trim(errmsg) == 0) errmsg = "field not set: " // trim(name)
    end if
  end function nml_percolation1_is_set

  !> \brief Validate required values and constraints
  integer function nml_percolation1_is_valid(this, errmsg) result(status)
    class(nml_percolation1_t), intent(in) :: this
    character(len=*), intent(out), optional :: errmsg
    integer :: istat

    status = NML_OK
    if (present(errmsg)) errmsg = ""

    ! required arrays
    if (all(ieee_is_nan(this%rechargeCoefficient))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: rechargeCoefficient"
      return
    end if
    if (any(ieee_is_nan(this%rechargeCoefficient))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: rechargeCoefficient"
      return
    end if
    if (all(ieee_is_nan(this%rechargeFactor_karstic))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: rechargeFactor_karstic"
      return
    end if
    if (any(ieee_is_nan(this%rechargeFactor_karstic))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: rechargeFactor_karstic"
      return
    end if
    if (all(ieee_is_nan(this%gain_loss_GWreservoir_karstic))) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: gain_loss_GWreservoir_karstic"
      return
    end if
    if (any(ieee_is_nan(this%gain_loss_GWreservoir_karstic))) then
      status = NML_ERR_PARTLY_SET
      if (present(errmsg)) errmsg = "array partly set: gain_loss_GWreservoir_karstic"
      return
    end if
  end function nml_percolation1_is_valid

end module nml_percolation1
