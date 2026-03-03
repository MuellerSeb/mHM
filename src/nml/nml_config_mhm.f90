!> \file nml_config_mhm.f90
!> \copydoc nml_config_mhm

!> \brief mHM model configuration
!> \details Configuration for the mHM model setup.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Jan 2026
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_namelists
module nml_config_mhm
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
  use ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_is_nan
  ! kind specifiers listed in the nml-tools configuration file
  use mo_kind, only: &
    dp

  implicit none

  ! default values
  logical, parameter, public :: read_restart_default = .false.
  logical, parameter, public :: read_restart_fluxes_default = .true.
  logical, parameter, public :: write_restart_default = .false.
  logical, parameter, public :: share_evap_coeff_default = .true.

  !> \class nml_config_mhm_t
  !> \brief mHM model configuration
  !> \details Configuration for the mHM model setup.
  type, public :: nml_config_mhm_t
    logical :: is_configured = .false. !< whether the namelist has been configured
    real(dp), dimension(max_domains) :: resolution !< mHM model resolution (L1)
    character(len=buf), dimension(max_domains) :: output_path !< Output path
    logical, dimension(max_domains) :: read_restart !< Read restart
    logical, dimension(max_domains) :: read_restart_fluxes !< Read restart fluxes
    character(len=buf), dimension(max_domains) :: restart_input_path !< Restart input path
    logical, dimension(max_domains) :: write_restart !< Write restart
    character(len=buf), dimension(max_domains) :: restart_output_path !< Restart output path
    real(dp), dimension(12, max_domains) :: evap_coeff !< Evaporation coefficients
    logical :: share_evap_coeff !< Share evaporation coefficients between domains
  contains
    procedure :: init => nml_config_mhm_init
    procedure :: from_file => nml_config_mhm_from_file
    procedure :: set => nml_config_mhm_set
    procedure :: is_set => nml_config_mhm_is_set
    procedure :: filled_shape => nml_config_mhm_filled_shape
    procedure :: is_valid => nml_config_mhm_is_valid
  end type nml_config_mhm_t

contains

  !> \brief Initialize defaults and sentinels for config_mhm
  integer function nml_config_mhm_init(this, errmsg) result(status)
    class(nml_config_mhm_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    this%is_configured = .false.

    ! sentinel values for required/optional parameters
    this%resolution = ieee_value(this%resolution, ieee_quiet_nan) ! sentinel for optional real array
    this%output_path = repeat(achar(0), len(this%output_path)) ! sentinel for optional string array
    this%restart_input_path = repeat(achar(0), len(this%restart_input_path)) ! sentinel for optional string array
    this%restart_output_path = repeat(achar(0), len(this%restart_output_path)) ! sentinel for optional string array
    this%evap_coeff = ieee_value(this%evap_coeff, ieee_quiet_nan) ! sentinel for optional real array
    ! default values
    this%read_restart = read_restart_default
    this%read_restart_fluxes = read_restart_fluxes_default
    this%write_restart = write_restart_default
    this%share_evap_coeff = share_evap_coeff_default ! bool values always need a default
  end function nml_config_mhm_init

  !> \brief Read config_mhm namelist from file
  integer function nml_config_mhm_from_file(this, file, errmsg) result(status)
    class(nml_config_mhm_t), intent(inout) :: this
    character(len=*), intent(in) :: file !< path to namelist file
    character(len=*), intent(out), optional :: errmsg
    ! namelist variables
    real(dp), dimension(max_domains) :: resolution
    character(len=buf), dimension(max_domains) :: output_path
    logical, dimension(max_domains) :: read_restart
    logical, dimension(max_domains) :: read_restart_fluxes
    character(len=buf), dimension(max_domains) :: restart_input_path
    logical, dimension(max_domains) :: write_restart
    character(len=buf), dimension(max_domains) :: restart_output_path
    real(dp), dimension(12, max_domains) :: evap_coeff
    logical :: share_evap_coeff
    ! locals
    type(nml_file_t) :: nml
    integer :: iostat
    integer :: close_status
    character(len=nml_line_buffer) :: iomsg

    namelist /config_mhm/ &
      resolution, &
      output_path, &
      read_restart, &
      read_restart_fluxes, &
      restart_input_path, &
      write_restart, &
      restart_output_path, &
      evap_coeff, &
      share_evap_coeff

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return
    resolution = this%resolution
    output_path = this%output_path
    read_restart = this%read_restart
    read_restart_fluxes = this%read_restart_fluxes
    restart_input_path = this%restart_input_path
    write_restart = this%write_restart
    restart_output_path = this%restart_output_path
    evap_coeff = this%evap_coeff
    share_evap_coeff = this%share_evap_coeff

    status = nml%open(file, errmsg=errmsg)
    if (status /= NML_OK) return

    status = nml%find("config_mhm", errmsg=errmsg)
    if (status /= NML_OK) then
      close_status = nml%close()
      return
    end if

    ! read namelist
    read(nml%unit, nml=config_mhm, iostat=iostat, iomsg=iomsg)
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
    this%resolution = resolution
    this%output_path = output_path
    this%read_restart = read_restart
    this%read_restart_fluxes = read_restart_fluxes
    this%restart_input_path = restart_input_path
    this%write_restart = write_restart
    this%restart_output_path = restart_output_path
    this%evap_coeff = evap_coeff
    this%share_evap_coeff = share_evap_coeff

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_config_mhm_from_file

  !> \brief Set config_mhm values
  integer function nml_config_mhm_set(this, &
    resolution, &
    output_path, &
    read_restart, &
    read_restart_fluxes, &
    restart_input_path, &
    write_restart, &
    restart_output_path, &
    evap_coeff, &
    share_evap_coeff, &
    errmsg) result(status)

    class(nml_config_mhm_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg
    real(dp), dimension(:), intent(in), optional :: resolution
    character(len=*), dimension(:), intent(in), optional :: output_path
    logical, dimension(:), intent(in), optional :: read_restart
    logical, dimension(:), intent(in), optional :: read_restart_fluxes
    character(len=*), dimension(:), intent(in), optional :: restart_input_path
    logical, dimension(:), intent(in), optional :: write_restart
    character(len=*), dimension(:), intent(in), optional :: restart_output_path
    real(dp), dimension(:, :), intent(in), optional :: evap_coeff
    logical, intent(in), optional :: share_evap_coeff
    integer :: &
      lb_1, &
      lb_2, &
      ub_1, &
      ub_2

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return

    ! required parameters
    ! override with provided values
    if (present(resolution)) then
      if (size(resolution, 1) > size(this%resolution, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'resolution'"
        return
      end if
      lb_1 = lbound(this%resolution, 1)
      ub_1 = lb_1 + size(resolution, 1) - 1
      this%resolution(lb_1:ub_1) = resolution
    end if
    if (present(output_path)) then
      if (size(output_path, 1) > size(this%output_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'output_path'"
        return
      end if
      lb_1 = lbound(this%output_path, 1)
      ub_1 = lb_1 + size(output_path, 1) - 1
      this%output_path(lb_1:ub_1) = output_path
    end if
    if (present(read_restart)) then
      if (size(read_restart, 1) > size(this%read_restart, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'read_restart'"
        return
      end if
      lb_1 = lbound(this%read_restart, 1)
      ub_1 = lb_1 + size(read_restart, 1) - 1
      this%read_restart(lb_1:ub_1) = read_restart
    end if
    if (present(read_restart_fluxes)) then
      if (size(read_restart_fluxes, 1) > size(this%read_restart_fluxes, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'read_restart_fluxes'"
        return
      end if
      lb_1 = lbound(this%read_restart_fluxes, 1)
      ub_1 = lb_1 + size(read_restart_fluxes, 1) - 1
      this%read_restart_fluxes(lb_1:ub_1) = read_restart_fluxes
    end if
    if (present(restart_input_path)) then
      if (size(restart_input_path, 1) > size(this%restart_input_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'restart_input_path'"
        return
      end if
      lb_1 = lbound(this%restart_input_path, 1)
      ub_1 = lb_1 + size(restart_input_path, 1) - 1
      this%restart_input_path(lb_1:ub_1) = restart_input_path
    end if
    if (present(write_restart)) then
      if (size(write_restart, 1) > size(this%write_restart, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'write_restart'"
        return
      end if
      lb_1 = lbound(this%write_restart, 1)
      ub_1 = lb_1 + size(write_restart, 1) - 1
      this%write_restart(lb_1:ub_1) = write_restart
    end if
    if (present(restart_output_path)) then
      if (size(restart_output_path, 1) > size(this%restart_output_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'restart_output_path'"
        return
      end if
      lb_1 = lbound(this%restart_output_path, 1)
      ub_1 = lb_1 + size(restart_output_path, 1) - 1
      this%restart_output_path(lb_1:ub_1) = restart_output_path
    end if
    if (present(evap_coeff)) then
      if (size(evap_coeff, 1) /= size(this%evap_coeff, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 mismatch for 'evap_coeff'"
        return
      end if
      if (size(evap_coeff, 2) > size(this%evap_coeff, 2)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 2 exceeds bounds for 'evap_coeff'"
        return
      end if
      lb_2 = lbound(this%evap_coeff, 2)
      ub_2 = lb_2 + size(evap_coeff, 2) - 1
      this%evap_coeff(:, lb_2:ub_2) = evap_coeff
    end if
    if (present(share_evap_coeff)) this%share_evap_coeff = share_evap_coeff

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_config_mhm_set

  !> \brief Check whether a namelist value was set
  integer function nml_config_mhm_is_set(this, name, idx, errmsg) result(status)
    class(nml_config_mhm_t), intent(in) :: this
    character(len=*), intent(in) :: name
    integer, intent(in), optional :: idx(:)
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    select case (to_lower(trim(name)))
    case ("resolution")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%resolution), ubound(this%resolution), &
          "resolution", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%resolution(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%resolution))) status = NML_ERR_NOT_SET
      end if
    case ("output_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%output_path), ubound(this%output_path), &
          "output_path", errmsg)
        if (status /= NML_OK) return
        if (this%output_path(idx(1)) == repeat(achar(0), len(this%output_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%output_path == repeat(achar(0), len(this%output_path)))) status = NML_ERR_NOT_SET
      end if
    case ("read_restart")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%read_restart), ubound(this%read_restart), &
          "read_restart", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("read_restart_fluxes")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%read_restart_fluxes), ubound(this%read_restart_fluxes), &
          "read_restart_fluxes", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("restart_input_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%restart_input_path), ubound(this%restart_input_path), &
          "restart_input_path", errmsg)
        if (status /= NML_OK) return
        if (this%restart_input_path(idx(1)) == repeat(achar(0), len(this%restart_input_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%restart_input_path == repeat(achar(0), len(this%restart_input_path)))) status = NML_ERR_NOT_SET
      end if
    case ("write_restart")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%write_restart), ubound(this%write_restart), &
          "write_restart", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("restart_output_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%restart_output_path), ubound(this%restart_output_path), &
          "restart_output_path", errmsg)
        if (status /= NML_OK) return
        if (this%restart_output_path(idx(1)) == repeat(achar(0), len(this%restart_output_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%restart_output_path == repeat(achar(0), len(this%restart_output_path)))) status = NML_ERR_NOT_SET
      end if
    case ("evap_coeff")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%evap_coeff), ubound(this%evap_coeff), &
          "evap_coeff", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%evap_coeff(idx(1), idx(2)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%evap_coeff))) status = NML_ERR_NOT_SET
      end if
    case ("share_evap_coeff")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'share_evap_coeff'"
        return
      end if
    case default
      status = NML_ERR_INVALID_NAME
      if (present(errmsg)) errmsg = "unknown field: " // trim(name)
    end select
    if (status == NML_ERR_NOT_SET .and. present(errmsg)) then
      if (len_trim(errmsg) == 0) errmsg = "field not set: " // trim(name)
    end if
  end function nml_config_mhm_is_set

  !> \brief Determine the filled shape along flexible dimensions
  integer function nml_config_mhm_filled_shape(this, name, filled, errmsg) result(status)
    class(nml_config_mhm_t), intent(in) :: this
    character(len=*), intent(in) :: name
    integer, intent(out) :: filled(:)
    character(len=*), intent(out), optional :: errmsg
    integer :: idx
    integer :: dim
    integer :: &
      lb_1, &
      lb_2, &
      ub_1, &
      ub_2

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    select case (to_lower(trim(name)))
    case ("resolution")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'resolution'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%resolution, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%resolution, 1), &
        lbound(this%resolution, 1), -1
        if (.not. (ieee_is_nan(this%resolution(idx)))) then
          filled(1) = idx - lbound(this%resolution, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%resolution, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(ieee_is_nan(this%resolution(lb_1:ub_1)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: resolution"
          return
        end if
      end if
    case ("output_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'output_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%output_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%output_path, 1), &
        lbound(this%output_path, 1), -1
        if (.not. (this%output_path(idx) == repeat(achar(0), len(this%output_path)))) then
          filled(1) = idx - lbound(this%output_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%output_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%output_path(lb_1:ub_1) == repeat(achar(0), len(this%output_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: output_path"
          return
        end if
      end if
    case ("restart_input_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'restart_input_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%restart_input_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%restart_input_path, 1), &
        lbound(this%restart_input_path, 1), -1
        if (.not. (this%restart_input_path(idx) == repeat(achar(0), len(this%restart_input_path)))) then
          filled(1) = idx - lbound(this%restart_input_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%restart_input_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%restart_input_path(lb_1:ub_1) == repeat(achar(0), len(this%restart_input_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: restart_input_path"
          return
        end if
      end if
    case ("restart_output_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'restart_output_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%restart_output_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%restart_output_path, 1), &
        lbound(this%restart_output_path, 1), -1
        if (.not. (this%restart_output_path(idx) == repeat(achar(0), len(this%restart_output_path)))) then
          filled(1) = idx - lbound(this%restart_output_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%restart_output_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%restart_output_path(lb_1:ub_1) == repeat(achar(0), len(this%restart_output_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: restart_output_path"
          return
        end if
      end if
    case ("evap_coeff")
      if (size(filled) /= 2) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'evap_coeff'"
        return
      end if
      do dim = 1, 2
        filled(dim) = size(this%evap_coeff, dim)
      end do
      filled(2) = 0
      do idx = ubound(this%evap_coeff, 2), &
        lbound(this%evap_coeff, 2), -1
        if (.not. (all(ieee_is_nan(this%evap_coeff(:, idx))))) then
          filled(2) = idx - lbound(this%evap_coeff, 2) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_2 = lbound(this%evap_coeff, 2)
        ub_2 = lb_2 + filled(2) - 1
        if (any(ieee_is_nan(this%evap_coeff(:, lb_2:ub_2)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: evap_coeff"
          return
        end if
      end if
    case default
      status = NML_ERR_INVALID_NAME
      if (present(errmsg)) errmsg = "field is not a flexible array: " // trim(name)
    end select
  end function nml_config_mhm_filled_shape

  !> \brief Validate required values and constraints
  integer function nml_config_mhm_is_valid(this, errmsg) result(status)
    class(nml_config_mhm_t), intent(in) :: this
    character(len=*), intent(out), optional :: errmsg
    integer :: istat
    integer, allocatable :: filled(:)

    status = NML_OK
    if (present(errmsg)) errmsg = ""

    ! flexible arrays
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("resolution", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: resolution"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("output_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: output_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("restart_input_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: restart_input_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("restart_output_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: restart_output_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(2))
    istat = this%filled_shape("evap_coeff", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: evap_coeff"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
  end function nml_config_mhm_is_valid

end module nml_config_mhm
