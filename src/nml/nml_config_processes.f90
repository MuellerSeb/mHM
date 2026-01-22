!> \file nml_config_processes.f90
!> \copydoc nml_config_processes

!> \brief Processes configuration
!> \details Configuration for process case selection in mHM.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Jan 2026
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_namelists
module nml_config_processes
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
    idx_check
  ! kind specifiers listed in the nml-tools configuration file
  use mo_kind, only: &
    i4

  implicit none

  ! default values
  integer(i4), parameter, public :: process_case_default = 0_i4

  !> \class nml_config_processes_t
  !> \brief Processes configuration
  !> \details Configuration for process case selection in mHM.
  type, public :: nml_config_processes_t
    logical :: is_configured = .false. !< whether the namelist has been configured
    integer(i4), dimension(11) :: process_case !< Process cases
  contains
    procedure :: init => nml_config_processes_init
    procedure :: from_file => nml_config_processes_from_file
    procedure :: set => nml_config_processes_set
    procedure :: is_set => nml_config_processes_is_set
    procedure :: is_valid => nml_config_processes_is_valid
  end type nml_config_processes_t

contains

  !> \brief Initialize defaults and sentinels for config_processes
  integer function nml_config_processes_init(this, errmsg) result(status)
    class(nml_config_processes_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    this%is_configured = .false.

    ! default values
    this%process_case = process_case_default
  end function nml_config_processes_init

  !> \brief Read config_processes namelist from file
  integer function nml_config_processes_from_file(this, file, errmsg) result(status)
    class(nml_config_processes_t), intent(inout) :: this
    character(len=*), intent(in) :: file !< path to namelist file
    character(len=*), intent(out), optional :: errmsg
    ! namelist variables
    integer(i4), dimension(11) :: process_case
    ! locals
    type(nml_file_t) :: nml
    integer :: iostat
    integer :: close_status
    character(len=nml_line_buffer) :: iomsg

    namelist /config_processes/ &
      process_case

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return
    process_case = this%process_case

    status = nml%open(file, errmsg=errmsg)
    if (status /= NML_OK) return

    status = nml%find("config_processes", errmsg=errmsg)
    if (status /= NML_OK) then
      close_status = nml%close()
      return
    end if

    ! read namelist
    read(nml%unit, nml=config_processes, iostat=iostat, iomsg=iomsg)
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
    this%process_case = process_case

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_config_processes_from_file

  !> \brief Set config_processes values
  integer function nml_config_processes_set(this, &
    process_case, &
    errmsg) result(status)

    class(nml_config_processes_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg
    integer(i4), dimension(:), intent(in), optional :: process_case
    integer :: &
      lb_1, &
      ub_1

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return

    ! required parameters
    ! override with provided values
    if (present(process_case)) then
      if (size(process_case, 1) > size(this%process_case, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'process_case'"
        return
      end if
      lb_1 = lbound(this%process_case, 1)
      ub_1 = lb_1 + size(process_case, 1) - 1
      this%process_case(lb_1:ub_1) = process_case
    end if

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_config_processes_set

  !> \brief Check whether a namelist value was set
  integer function nml_config_processes_is_set(this, name, idx, errmsg) result(status)
    class(nml_config_processes_t), intent(in) :: this
    character(len=*), intent(in) :: name
    integer, intent(in), optional :: idx(:)
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    select case (trim(name))
    case ("process_case")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%process_case), ubound(this%process_case), &
          "process_case", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case default
      status = NML_ERR_INVALID_NAME
      if (present(errmsg)) errmsg = "unknown field: " // trim(name)
    end select
    if (status == NML_ERR_NOT_SET .and. present(errmsg)) then
      if (len_trim(errmsg) == 0) errmsg = "field not set: " // trim(name)
    end if
  end function nml_config_processes_is_set

  !> \brief Validate required values and constraints
  integer function nml_config_processes_is_valid(this, errmsg) result(status)
    class(nml_config_processes_t), intent(in) :: this
    character(len=*), intent(out), optional :: errmsg
    integer :: istat

    status = NML_OK
    if (present(errmsg)) errmsg = ""

  end function nml_config_processes_is_valid

end module nml_config_processes
