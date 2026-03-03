!> \file nml_config_time.f90
!> \copydoc nml_config_time

!> \brief Time configuration
!> \details Configuration for simulation and evaluation time periods in mHM.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Jan 2026
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_namelists
module nml_config_time
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
    max_domains, &
    buf, &
    NML_ERR_PARTLY_SET
  ! kind specifiers listed in the nml-tools configuration file
  use mo_kind, only: &
    i4

  implicit none

  ! default values
  logical, parameter, public :: share_time_period_default = .false.
  integer(i4), parameter, public :: time_step_default = 1_i4
  logical, parameter, public :: share_time_step_default = .true.

  !> \class nml_config_time_t
  !> \brief Time configuration
  !> \details Configuration for simulation and evaluation time periods in mHM.
  type, public :: nml_config_time_t
    logical :: is_configured = .false. !< whether the namelist has been configured
    character(len=buf), dimension(max_domains) :: sim_start !< Simulation start
    character(len=buf), dimension(max_domains) :: eval_start !< Evaluation start
    character(len=buf), dimension(max_domains) :: sim_end !< Simulation end
    logical :: share_time_period !< Share time period between domains
    integer(i4), dimension(max_domains) :: time_step !< Time step of the simulation
    logical :: share_time_step !< Share time step between domains
  contains
    procedure :: init => nml_config_time_init
    procedure :: from_file => nml_config_time_from_file
    procedure :: set => nml_config_time_set
    procedure :: is_set => nml_config_time_is_set
    procedure :: filled_shape => nml_config_time_filled_shape
    procedure :: is_valid => nml_config_time_is_valid
  end type nml_config_time_t

contains

  !> \brief Initialize defaults and sentinels for config_time
  integer function nml_config_time_init(this, errmsg) result(status)
    class(nml_config_time_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    this%is_configured = .false.

    ! sentinel values for required/optional parameters
    this%sim_start = repeat(achar(0), len(this%sim_start)) ! sentinel for optional string array
    this%eval_start = repeat(achar(0), len(this%eval_start)) ! sentinel for optional string array
    this%sim_end = repeat(achar(0), len(this%sim_end)) ! sentinel for optional string array
    ! default values
    this%share_time_period = share_time_period_default ! bool values always need a default
    this%time_step = time_step_default
    this%share_time_step = share_time_step_default ! bool values always need a default
  end function nml_config_time_init

  !> \brief Read config_time namelist from file
  integer function nml_config_time_from_file(this, file, errmsg) result(status)
    class(nml_config_time_t), intent(inout) :: this
    character(len=*), intent(in) :: file !< path to namelist file
    character(len=*), intent(out), optional :: errmsg
    ! namelist variables
    character(len=buf), dimension(max_domains) :: sim_start
    character(len=buf), dimension(max_domains) :: eval_start
    character(len=buf), dimension(max_domains) :: sim_end
    logical :: share_time_period
    integer(i4), dimension(max_domains) :: time_step
    logical :: share_time_step
    ! locals
    type(nml_file_t) :: nml
    integer :: iostat
    integer :: close_status
    character(len=nml_line_buffer) :: iomsg

    namelist /config_time/ &
      sim_start, &
      eval_start, &
      sim_end, &
      share_time_period, &
      time_step, &
      share_time_step

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return
    sim_start = this%sim_start
    eval_start = this%eval_start
    sim_end = this%sim_end
    share_time_period = this%share_time_period
    time_step = this%time_step
    share_time_step = this%share_time_step

    status = nml%open(file, errmsg=errmsg)
    if (status /= NML_OK) return

    status = nml%find("config_time", errmsg=errmsg)
    if (status /= NML_OK) then
      close_status = nml%close()
      return
    end if

    ! read namelist
    read(nml%unit, nml=config_time, iostat=iostat, iomsg=iomsg)
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
    this%sim_start = sim_start
    this%eval_start = eval_start
    this%sim_end = sim_end
    this%share_time_period = share_time_period
    this%time_step = time_step
    this%share_time_step = share_time_step

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_config_time_from_file

  !> \brief Set config_time values
  integer function nml_config_time_set(this, &
    sim_start, &
    eval_start, &
    sim_end, &
    share_time_period, &
    time_step, &
    share_time_step, &
    errmsg) result(status)

    class(nml_config_time_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg
    character(len=*), dimension(:), intent(in), optional :: sim_start
    character(len=*), dimension(:), intent(in), optional :: eval_start
    character(len=*), dimension(:), intent(in), optional :: sim_end
    logical, intent(in), optional :: share_time_period
    integer(i4), dimension(:), intent(in), optional :: time_step
    logical, intent(in), optional :: share_time_step
    integer :: &
      lb_1, &
      ub_1

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return

    ! required parameters
    ! override with provided values
    if (present(sim_start)) then
      if (size(sim_start, 1) > size(this%sim_start, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'sim_start'"
        return
      end if
      lb_1 = lbound(this%sim_start, 1)
      ub_1 = lb_1 + size(sim_start, 1) - 1
      this%sim_start(lb_1:ub_1) = sim_start
    end if
    if (present(eval_start)) then
      if (size(eval_start, 1) > size(this%eval_start, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'eval_start'"
        return
      end if
      lb_1 = lbound(this%eval_start, 1)
      ub_1 = lb_1 + size(eval_start, 1) - 1
      this%eval_start(lb_1:ub_1) = eval_start
    end if
    if (present(sim_end)) then
      if (size(sim_end, 1) > size(this%sim_end, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'sim_end'"
        return
      end if
      lb_1 = lbound(this%sim_end, 1)
      ub_1 = lb_1 + size(sim_end, 1) - 1
      this%sim_end(lb_1:ub_1) = sim_end
    end if
    if (present(share_time_period)) this%share_time_period = share_time_period
    if (present(time_step)) then
      if (size(time_step, 1) > size(this%time_step, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'time_step'"
        return
      end if
      lb_1 = lbound(this%time_step, 1)
      ub_1 = lb_1 + size(time_step, 1) - 1
      this%time_step(lb_1:ub_1) = time_step
    end if
    if (present(share_time_step)) this%share_time_step = share_time_step

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_config_time_set

  !> \brief Check whether a namelist value was set
  integer function nml_config_time_is_set(this, name, idx, errmsg) result(status)
    class(nml_config_time_t), intent(in) :: this
    character(len=*), intent(in) :: name
    integer, intent(in), optional :: idx(:)
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    select case (to_lower(trim(name)))
    case ("sim_start")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%sim_start), ubound(this%sim_start), &
          "sim_start", errmsg)
        if (status /= NML_OK) return
        if (this%sim_start(idx(1)) == repeat(achar(0), len(this%sim_start))) status = NML_ERR_NOT_SET
      else
        if (all(this%sim_start == repeat(achar(0), len(this%sim_start)))) status = NML_ERR_NOT_SET
      end if
    case ("eval_start")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%eval_start), ubound(this%eval_start), &
          "eval_start", errmsg)
        if (status /= NML_OK) return
        if (this%eval_start(idx(1)) == repeat(achar(0), len(this%eval_start))) status = NML_ERR_NOT_SET
      else
        if (all(this%eval_start == repeat(achar(0), len(this%eval_start)))) status = NML_ERR_NOT_SET
      end if
    case ("sim_end")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%sim_end), ubound(this%sim_end), &
          "sim_end", errmsg)
        if (status /= NML_OK) return
        if (this%sim_end(idx(1)) == repeat(achar(0), len(this%sim_end))) status = NML_ERR_NOT_SET
      else
        if (all(this%sim_end == repeat(achar(0), len(this%sim_end)))) status = NML_ERR_NOT_SET
      end if
    case ("share_time_period")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'share_time_period'"
        return
      end if
    case ("time_step")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%time_step), ubound(this%time_step), &
          "time_step", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("share_time_step")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'share_time_step'"
        return
      end if
    case default
      status = NML_ERR_INVALID_NAME
      if (present(errmsg)) errmsg = "unknown field: " // trim(name)
    end select
    if (status == NML_ERR_NOT_SET .and. present(errmsg)) then
      if (len_trim(errmsg) == 0) errmsg = "field not set: " // trim(name)
    end if
  end function nml_config_time_is_set

  !> \brief Determine the filled shape along flexible dimensions
  integer function nml_config_time_filled_shape(this, name, filled, errmsg) result(status)
    class(nml_config_time_t), intent(in) :: this
    character(len=*), intent(in) :: name
    integer, intent(out) :: filled(:)
    character(len=*), intent(out), optional :: errmsg
    integer :: idx
    integer :: dim
    integer :: &
      lb_1, &
      ub_1

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    select case (to_lower(trim(name)))
    case ("sim_start")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'sim_start'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%sim_start, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%sim_start, 1), &
        lbound(this%sim_start, 1), -1
        if (.not. (this%sim_start(idx) == repeat(achar(0), len(this%sim_start)))) then
          filled(1) = idx - lbound(this%sim_start, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%sim_start, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%sim_start(lb_1:ub_1) == repeat(achar(0), len(this%sim_start)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: sim_start"
          return
        end if
      end if
    case ("eval_start")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'eval_start'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%eval_start, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%eval_start, 1), &
        lbound(this%eval_start, 1), -1
        if (.not. (this%eval_start(idx) == repeat(achar(0), len(this%eval_start)))) then
          filled(1) = idx - lbound(this%eval_start, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%eval_start, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%eval_start(lb_1:ub_1) == repeat(achar(0), len(this%eval_start)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: eval_start"
          return
        end if
      end if
    case ("sim_end")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'sim_end'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%sim_end, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%sim_end, 1), &
        lbound(this%sim_end, 1), -1
        if (.not. (this%sim_end(idx) == repeat(achar(0), len(this%sim_end)))) then
          filled(1) = idx - lbound(this%sim_end, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%sim_end, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%sim_end(lb_1:ub_1) == repeat(achar(0), len(this%sim_end)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: sim_end"
          return
        end if
      end if
    case default
      status = NML_ERR_INVALID_NAME
      if (present(errmsg)) errmsg = "field is not a flexible array: " // trim(name)
    end select
  end function nml_config_time_filled_shape

  !> \brief Validate required values and constraints
  integer function nml_config_time_is_valid(this, errmsg) result(status)
    class(nml_config_time_t), intent(in) :: this
    character(len=*), intent(out), optional :: errmsg
    integer :: istat
    integer, allocatable :: filled(:)

    status = NML_OK
    if (present(errmsg)) errmsg = ""

    ! flexible arrays
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("sim_start", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: sim_start"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("eval_start", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: eval_start"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("sim_end", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: sim_end"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
  end function nml_config_time_is_valid

end module nml_config_time
