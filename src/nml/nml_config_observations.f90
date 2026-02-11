!> \file nml_config_observations.f90
!> \copydoc nml_config_observations

!> \brief Observations configuration
!> \details Configuration for observation input data in mHM.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Jan 2026
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_namelists
module nml_config_observations
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
    dp, &
    i4

  implicit none

  !> \class nml_config_observations_t
  !> \brief Observations configuration
  !> \details Configuration for observation input data in mHM.
  type, public :: nml_config_observations_t
    logical :: is_configured = .false. !< whether the namelist has been configured
    character(len=buf), dimension(max_domains) :: sm_path !< Soil moisture data path
    character(len=buf), dimension(max_domains) :: neutrons_path !< Neutron data path
    character(len=buf), dimension(max_domains) :: et_path !< Evapotranspiration data path
    character(len=buf), dimension(max_domains) :: tws_path !< Domain average TWS path
    real(dp), dimension(max_domains) :: bfi_obs !< Baseflow index per domain
    integer(i4) :: sm_horizons !< Number of mHM soil moisture horizons
    integer(i4) :: sm_time_step !< Time step of soil moisture
    integer(i4) :: et_time_step !< Time step of evapotranspiration
    integer(i4) :: tws_time_step !< Time step of total water storage
  contains
    procedure :: init => nml_config_observations_init
    procedure :: from_file => nml_config_observations_from_file
    procedure :: set => nml_config_observations_set
    procedure :: is_set => nml_config_observations_is_set
    procedure :: filled_shape => nml_config_observations_filled_shape
    procedure :: is_valid => nml_config_observations_is_valid
  end type nml_config_observations_t

contains

  !> \brief Initialize defaults and sentinels for config_observations
  integer function nml_config_observations_init(this, errmsg) result(status)
    class(nml_config_observations_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    this%is_configured = .false.

    ! sentinel values for required/optional parameters
    this%sm_path = repeat(achar(0), len(this%sm_path)) ! sentinel for optional string array
    this%neutrons_path = repeat(achar(0), len(this%neutrons_path)) ! sentinel for optional string array
    this%et_path = repeat(achar(0), len(this%et_path)) ! sentinel for optional string array
    this%tws_path = repeat(achar(0), len(this%tws_path)) ! sentinel for optional string array
    this%bfi_obs = ieee_value(this%bfi_obs, ieee_quiet_nan) ! sentinel for optional real array
    this%sm_horizons = -huge(this%sm_horizons) ! sentinel for optional integer
    this%sm_time_step = -huge(this%sm_time_step) ! sentinel for optional integer
    this%et_time_step = -huge(this%et_time_step) ! sentinel for optional integer
    this%tws_time_step = -huge(this%tws_time_step) ! sentinel for optional integer
  end function nml_config_observations_init

  !> \brief Read config_observations namelist from file
  integer function nml_config_observations_from_file(this, file, errmsg) result(status)
    class(nml_config_observations_t), intent(inout) :: this
    character(len=*), intent(in) :: file !< path to namelist file
    character(len=*), intent(out), optional :: errmsg
    ! namelist variables
    character(len=buf), dimension(max_domains) :: sm_path
    character(len=buf), dimension(max_domains) :: neutrons_path
    character(len=buf), dimension(max_domains) :: et_path
    character(len=buf), dimension(max_domains) :: tws_path
    real(dp), dimension(max_domains) :: bfi_obs
    integer(i4) :: sm_horizons
    integer(i4) :: sm_time_step
    integer(i4) :: et_time_step
    integer(i4) :: tws_time_step
    ! locals
    type(nml_file_t) :: nml
    integer :: iostat
    integer :: close_status
    character(len=nml_line_buffer) :: iomsg

    namelist /config_observations/ &
      sm_path, &
      neutrons_path, &
      et_path, &
      tws_path, &
      bfi_obs, &
      sm_horizons, &
      sm_time_step, &
      et_time_step, &
      tws_time_step

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return
    sm_path = this%sm_path
    neutrons_path = this%neutrons_path
    et_path = this%et_path
    tws_path = this%tws_path
    bfi_obs = this%bfi_obs
    sm_horizons = this%sm_horizons
    sm_time_step = this%sm_time_step
    et_time_step = this%et_time_step
    tws_time_step = this%tws_time_step

    status = nml%open(file, errmsg=errmsg)
    if (status /= NML_OK) return

    status = nml%find("config_observations", errmsg=errmsg)
    if (status /= NML_OK) then
      close_status = nml%close()
      return
    end if

    ! read namelist
    read(nml%unit, nml=config_observations, iostat=iostat, iomsg=iomsg)
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
    this%sm_path = sm_path
    this%neutrons_path = neutrons_path
    this%et_path = et_path
    this%tws_path = tws_path
    this%bfi_obs = bfi_obs
    this%sm_horizons = sm_horizons
    this%sm_time_step = sm_time_step
    this%et_time_step = et_time_step
    this%tws_time_step = tws_time_step

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_config_observations_from_file

  !> \brief Set config_observations values
  integer function nml_config_observations_set(this, &
    sm_path, &
    neutrons_path, &
    et_path, &
    tws_path, &
    bfi_obs, &
    sm_horizons, &
    sm_time_step, &
    et_time_step, &
    tws_time_step, &
    errmsg) result(status)

    class(nml_config_observations_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg
    character(len=*), dimension(:), intent(in), optional :: sm_path
    character(len=*), dimension(:), intent(in), optional :: neutrons_path
    character(len=*), dimension(:), intent(in), optional :: et_path
    character(len=*), dimension(:), intent(in), optional :: tws_path
    real(dp), dimension(:), intent(in), optional :: bfi_obs
    integer(i4), intent(in), optional :: sm_horizons
    integer(i4), intent(in), optional :: sm_time_step
    integer(i4), intent(in), optional :: et_time_step
    integer(i4), intent(in), optional :: tws_time_step
    integer :: &
      lb_1, &
      ub_1

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return

    ! required parameters
    ! override with provided values
    if (present(sm_path)) then
      if (size(sm_path, 1) > size(this%sm_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'sm_path'"
        return
      end if
      lb_1 = lbound(this%sm_path, 1)
      ub_1 = lb_1 + size(sm_path, 1) - 1
      this%sm_path(lb_1:ub_1) = sm_path
    end if
    if (present(neutrons_path)) then
      if (size(neutrons_path, 1) > size(this%neutrons_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'neutrons_path'"
        return
      end if
      lb_1 = lbound(this%neutrons_path, 1)
      ub_1 = lb_1 + size(neutrons_path, 1) - 1
      this%neutrons_path(lb_1:ub_1) = neutrons_path
    end if
    if (present(et_path)) then
      if (size(et_path, 1) > size(this%et_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'et_path'"
        return
      end if
      lb_1 = lbound(this%et_path, 1)
      ub_1 = lb_1 + size(et_path, 1) - 1
      this%et_path(lb_1:ub_1) = et_path
    end if
    if (present(tws_path)) then
      if (size(tws_path, 1) > size(this%tws_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'tws_path'"
        return
      end if
      lb_1 = lbound(this%tws_path, 1)
      ub_1 = lb_1 + size(tws_path, 1) - 1
      this%tws_path(lb_1:ub_1) = tws_path
    end if
    if (present(bfi_obs)) then
      if (size(bfi_obs, 1) > size(this%bfi_obs, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'bfi_obs'"
        return
      end if
      lb_1 = lbound(this%bfi_obs, 1)
      ub_1 = lb_1 + size(bfi_obs, 1) - 1
      this%bfi_obs(lb_1:ub_1) = bfi_obs
    end if
    if (present(sm_horizons)) this%sm_horizons = sm_horizons
    if (present(sm_time_step)) this%sm_time_step = sm_time_step
    if (present(et_time_step)) this%et_time_step = et_time_step
    if (present(tws_time_step)) this%tws_time_step = tws_time_step

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_config_observations_set

  !> \brief Check whether a namelist value was set
  integer function nml_config_observations_is_set(this, name, idx, errmsg) result(status)
    class(nml_config_observations_t), intent(in) :: this
    character(len=*), intent(in) :: name
    integer, intent(in), optional :: idx(:)
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    select case (to_lower(trim(name)))
    case ("sm_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%sm_path), ubound(this%sm_path), &
          "sm_path", errmsg)
        if (status /= NML_OK) return
        if (this%sm_path(idx(1)) == repeat(achar(0), len(this%sm_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%sm_path == repeat(achar(0), len(this%sm_path)))) status = NML_ERR_NOT_SET
      end if
    case ("neutrons_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%neutrons_path), ubound(this%neutrons_path), &
          "neutrons_path", errmsg)
        if (status /= NML_OK) return
        if (this%neutrons_path(idx(1)) == repeat(achar(0), len(this%neutrons_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%neutrons_path == repeat(achar(0), len(this%neutrons_path)))) status = NML_ERR_NOT_SET
      end if
    case ("et_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%et_path), ubound(this%et_path), &
          "et_path", errmsg)
        if (status /= NML_OK) return
        if (this%et_path(idx(1)) == repeat(achar(0), len(this%et_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%et_path == repeat(achar(0), len(this%et_path)))) status = NML_ERR_NOT_SET
      end if
    case ("tws_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%tws_path), ubound(this%tws_path), &
          "tws_path", errmsg)
        if (status /= NML_OK) return
        if (this%tws_path(idx(1)) == repeat(achar(0), len(this%tws_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%tws_path == repeat(achar(0), len(this%tws_path)))) status = NML_ERR_NOT_SET
      end if
    case ("bfi_obs")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%bfi_obs), ubound(this%bfi_obs), &
          "BFI_obs", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%bfi_obs(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%bfi_obs))) status = NML_ERR_NOT_SET
      end if
    case ("sm_horizons")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'sm_horizons'"
        return
      end if
      if (this%sm_horizons == -huge(this%sm_horizons)) status = NML_ERR_NOT_SET
    case ("sm_time_step")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'sm_time_step'"
        return
      end if
      if (this%sm_time_step == -huge(this%sm_time_step)) status = NML_ERR_NOT_SET
    case ("et_time_step")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'et_time_step'"
        return
      end if
      if (this%et_time_step == -huge(this%et_time_step)) status = NML_ERR_NOT_SET
    case ("tws_time_step")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'tws_time_step'"
        return
      end if
      if (this%tws_time_step == -huge(this%tws_time_step)) status = NML_ERR_NOT_SET
    case default
      status = NML_ERR_INVALID_NAME
      if (present(errmsg)) errmsg = "unknown field: " // trim(name)
    end select
    if (status == NML_ERR_NOT_SET .and. present(errmsg)) then
      if (len_trim(errmsg) == 0) errmsg = "field not set: " // trim(name)
    end if
  end function nml_config_observations_is_set

  !> \brief Determine the filled shape along flexible dimensions
  integer function nml_config_observations_filled_shape(this, name, filled, errmsg) result(status)
    class(nml_config_observations_t), intent(in) :: this
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
    case ("sm_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'sm_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%sm_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%sm_path, 1), &
        lbound(this%sm_path, 1), -1
        if (.not. (this%sm_path(idx) == repeat(achar(0), len(this%sm_path)))) then
          filled(1) = idx - lbound(this%sm_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%sm_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%sm_path(lb_1:ub_1) == repeat(achar(0), len(this%sm_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: sm_path"
          return
        end if
      end if
    case ("neutrons_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'neutrons_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%neutrons_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%neutrons_path, 1), &
        lbound(this%neutrons_path, 1), -1
        if (.not. (this%neutrons_path(idx) == repeat(achar(0), len(this%neutrons_path)))) then
          filled(1) = idx - lbound(this%neutrons_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%neutrons_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%neutrons_path(lb_1:ub_1) == repeat(achar(0), len(this%neutrons_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: neutrons_path"
          return
        end if
      end if
    case ("et_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'et_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%et_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%et_path, 1), &
        lbound(this%et_path, 1), -1
        if (.not. (this%et_path(idx) == repeat(achar(0), len(this%et_path)))) then
          filled(1) = idx - lbound(this%et_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%et_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%et_path(lb_1:ub_1) == repeat(achar(0), len(this%et_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: et_path"
          return
        end if
      end if
    case ("tws_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'tws_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%tws_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%tws_path, 1), &
        lbound(this%tws_path, 1), -1
        if (.not. (this%tws_path(idx) == repeat(achar(0), len(this%tws_path)))) then
          filled(1) = idx - lbound(this%tws_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%tws_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%tws_path(lb_1:ub_1) == repeat(achar(0), len(this%tws_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: tws_path"
          return
        end if
      end if
    case ("bfi_obs")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'BFI_obs'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%bfi_obs, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%bfi_obs, 1), &
        lbound(this%bfi_obs, 1), -1
        if (.not. (ieee_is_nan(this%bfi_obs(idx)))) then
          filled(1) = idx - lbound(this%bfi_obs, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%bfi_obs, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(ieee_is_nan(this%bfi_obs(lb_1:ub_1)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: BFI_obs"
          return
        end if
      end if
    case default
      status = NML_ERR_INVALID_NAME
      if (present(errmsg)) errmsg = "field is not a flexible array: " // trim(name)
    end select
  end function nml_config_observations_filled_shape

  !> \brief Validate required values and constraints
  integer function nml_config_observations_is_valid(this, errmsg) result(status)
    class(nml_config_observations_t), intent(in) :: this
    character(len=*), intent(out), optional :: errmsg
    integer :: istat
    integer, allocatable :: filled(:)

    status = NML_OK
    if (present(errmsg)) errmsg = ""

    ! flexible arrays
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("sm_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: sm_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("neutrons_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: neutrons_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("et_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: et_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("tws_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: tws_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("BFI_obs", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: BFI_obs"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
  end function nml_config_observations_is_valid

end module nml_config_observations
