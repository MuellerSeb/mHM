!> \file nml_config_mrm.f90
!> \copydoc nml_config_mrm

!> \brief mRM configuration
!> \details Configuration for the multi-scale routing model (mRM) in mHM.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Jan 2026
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_namelists
module nml_config_mrm
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
    max_domains, &
    buf, &
    NML_ERR_PARTLY_SET
  ! kind specifiers listed in the nml-tools configuration file
  use mo_kind, only: &
    i4, &
    dp

  implicit none

  ! default values
  logical, parameter, public :: read_restart_default = .false.
  logical, parameter, public :: read_restart_fluxes_default = .true.
  logical, parameter, public :: write_restart_default = .false.
  real(dp), parameter, public :: albedo_water_default = 0.15_dp
  real(dp), parameter, public :: pt_a_water_default = 1.26_dp
  real(dp), parameter, public :: emissivity_water_default = 0.96_dp
  real(dp), parameter, public :: turb_heat_ex_coeff_default = 20.0_dp
  integer(i4), parameter, public :: max_iter_default = 50_i4
  real(dp), parameter, public :: delta_iter_default = 0.01_dp
  real(dp), parameter, public :: step_iter_default = 5.0_dp

  !> \class nml_config_mrm_t
  !> \brief mRM configuration
  !> \details Configuration for the multi-scale routing model (mRM) in mHM.
  type, public :: nml_config_mrm_t
    logical :: is_configured = .false. !< whether the namelist has been configured
    integer(i4), dimension(max_domains) :: resolution !< mRM resolution
    character(len=buf), dimension(max_domains) :: output_path !< Output path
    logical, dimension(max_domains) :: read_restart !< Read restart
    logical, dimension(max_domains) :: read_restart_fluxes !< Read restart fluxes
    character(len=buf), dimension(max_domains) :: restart_input_path !< Restart input path
    logical, dimension(max_domains) :: write_restart !< Write restart
    character(len=buf), dimension(max_domains) :: restart_output_path !< Restart output path
    real(dp) :: albedo_water !< Albedo of open water
    real(dp) :: pt_a_water !< Priestley-Taylor alpha
    real(dp) :: emissivity_water !< Emissivity of water
    real(dp) :: turb_heat_ex_coeff !< Turbulent heat exchange coefficient
    integer(i4) :: max_iter !< Max number of iterations
    real(dp) :: delta_iter !< Convergence criterion iteration
    real(dp) :: step_iter !< Maximal step in iteration
  contains
    procedure :: init => nml_config_mrm_init
    procedure :: from_file => nml_config_mrm_from_file
    procedure :: set => nml_config_mrm_set
    procedure :: is_set => nml_config_mrm_is_set
    procedure :: filled_shape => nml_config_mrm_filled_shape
    procedure :: is_valid => nml_config_mrm_is_valid
  end type nml_config_mrm_t

contains

  !> \brief Initialize defaults and sentinels for config_mrm
  integer function nml_config_mrm_init(this, errmsg) result(status)
    class(nml_config_mrm_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    this%is_configured = .false.

    ! sentinel values for required/optional parameters
    this%resolution = -huge(this%resolution) ! sentinel for required integer array
    this%output_path = repeat(achar(0), len(this%output_path)) ! sentinel for optional string array
    this%restart_input_path = repeat(achar(0), len(this%restart_input_path)) ! sentinel for optional string array
    this%restart_output_path = repeat(achar(0), len(this%restart_output_path)) ! sentinel for optional string array
    ! default values
    this%read_restart = read_restart_default
    this%read_restart_fluxes = read_restart_fluxes_default
    this%write_restart = write_restart_default
    this%albedo_water = albedo_water_default
    this%pt_a_water = pt_a_water_default
    this%emissivity_water = emissivity_water_default
    this%turb_heat_ex_coeff = turb_heat_ex_coeff_default
    this%max_iter = max_iter_default
    this%delta_iter = delta_iter_default
    this%step_iter = step_iter_default
  end function nml_config_mrm_init

  !> \brief Read config_mrm namelist from file
  integer function nml_config_mrm_from_file(this, file, errmsg) result(status)
    class(nml_config_mrm_t), intent(inout) :: this
    character(len=*), intent(in) :: file !< path to namelist file
    character(len=*), intent(out), optional :: errmsg
    ! namelist variables
    integer(i4), dimension(max_domains) :: resolution
    character(len=buf), dimension(max_domains) :: output_path
    logical, dimension(max_domains) :: read_restart
    logical, dimension(max_domains) :: read_restart_fluxes
    character(len=buf), dimension(max_domains) :: restart_input_path
    logical, dimension(max_domains) :: write_restart
    character(len=buf), dimension(max_domains) :: restart_output_path
    real(dp) :: albedo_water
    real(dp) :: pt_a_water
    real(dp) :: emissivity_water
    real(dp) :: turb_heat_ex_coeff
    integer(i4) :: max_iter
    real(dp) :: delta_iter
    real(dp) :: step_iter
    ! locals
    type(nml_file_t) :: nml
    integer :: iostat
    integer :: close_status
    character(len=nml_line_buffer) :: iomsg

    namelist /config_mrm/ &
      resolution, &
      output_path, &
      read_restart, &
      read_restart_fluxes, &
      restart_input_path, &
      write_restart, &
      restart_output_path, &
      albedo_water, &
      pt_a_water, &
      emissivity_water, &
      turb_heat_ex_coeff, &
      max_iter, &
      delta_iter, &
      step_iter

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return
    resolution = this%resolution
    output_path = this%output_path
    read_restart = this%read_restart
    read_restart_fluxes = this%read_restart_fluxes
    restart_input_path = this%restart_input_path
    write_restart = this%write_restart
    restart_output_path = this%restart_output_path
    albedo_water = this%albedo_water
    pt_a_water = this%pt_a_water
    emissivity_water = this%emissivity_water
    turb_heat_ex_coeff = this%turb_heat_ex_coeff
    max_iter = this%max_iter
    delta_iter = this%delta_iter
    step_iter = this%step_iter

    status = nml%open(file, errmsg=errmsg)
    if (status /= NML_OK) return

    status = nml%find("config_mrm", errmsg=errmsg)
    if (status /= NML_OK) then
      close_status = nml%close()
      return
    end if

    ! read namelist
    read(nml%unit, nml=config_mrm, iostat=iostat, iomsg=iomsg)
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
    this%albedo_water = albedo_water
    this%pt_a_water = pt_a_water
    this%emissivity_water = emissivity_water
    this%turb_heat_ex_coeff = turb_heat_ex_coeff
    this%max_iter = max_iter
    this%delta_iter = delta_iter
    this%step_iter = step_iter

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_config_mrm_from_file

  !> \brief Set config_mrm values
  integer function nml_config_mrm_set(this, &
    resolution, &
    output_path, &
    read_restart, &
    read_restart_fluxes, &
    restart_input_path, &
    write_restart, &
    restart_output_path, &
    albedo_water, &
    pt_a_water, &
    emissivity_water, &
    turb_heat_ex_coeff, &
    max_iter, &
    delta_iter, &
    step_iter, &
    errmsg) result(status)

    class(nml_config_mrm_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg
    integer(i4), dimension(:), intent(in) :: resolution
    character(len=*), dimension(:), intent(in), optional :: output_path
    logical, dimension(:), intent(in), optional :: read_restart
    logical, dimension(:), intent(in), optional :: read_restart_fluxes
    character(len=*), dimension(:), intent(in), optional :: restart_input_path
    logical, dimension(:), intent(in), optional :: write_restart
    character(len=*), dimension(:), intent(in), optional :: restart_output_path
    real(dp), intent(in), optional :: albedo_water
    real(dp), intent(in), optional :: pt_a_water
    real(dp), intent(in), optional :: emissivity_water
    real(dp), intent(in), optional :: turb_heat_ex_coeff
    integer(i4), intent(in), optional :: max_iter
    real(dp), intent(in), optional :: delta_iter
    real(dp), intent(in), optional :: step_iter
    integer :: &
      lb_1, &
      ub_1

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return

    ! required parameters
    if (size(resolution, 1) > size(this%resolution, 1)) then
      status = NML_ERR_INVALID_INDEX
      if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'resolution'"
      return
    end if
    lb_1 = lbound(this%resolution, 1)
    ub_1 = lb_1 + size(resolution, 1) - 1
    this%resolution(lb_1:ub_1) = resolution
    ! override with provided values
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
    if (present(albedo_water)) this%albedo_water = albedo_water
    if (present(pt_a_water)) this%pt_a_water = pt_a_water
    if (present(emissivity_water)) this%emissivity_water = emissivity_water
    if (present(turb_heat_ex_coeff)) this%turb_heat_ex_coeff = turb_heat_ex_coeff
    if (present(max_iter)) this%max_iter = max_iter
    if (present(delta_iter)) this%delta_iter = delta_iter
    if (present(step_iter)) this%step_iter = step_iter

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_config_mrm_set

  !> \brief Check whether a namelist value was set
  integer function nml_config_mrm_is_set(this, name, idx, errmsg) result(status)
    class(nml_config_mrm_t), intent(in) :: this
    character(len=*), intent(in) :: name
    integer, intent(in), optional :: idx(:)
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    select case (trim(name))
    case ("resolution")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%resolution), ubound(this%resolution), &
          "resolution", errmsg)
        if (status /= NML_OK) return
        if (this%resolution(idx(1)) == -huge(this%resolution(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(this%resolution == -huge(this%resolution))) status = NML_ERR_NOT_SET
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
    case ("albedo_water")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'albedo_water'"
        return
      end if
    case ("pt_a_water")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'pt_a_water'"
        return
      end if
    case ("emissivity_water")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'emissivity_water'"
        return
      end if
    case ("turb_heat_ex_coeff")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'turb_heat_ex_coeff'"
        return
      end if
    case ("max_iter")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'max_iter'"
        return
      end if
    case ("delta_iter")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'delta_iter'"
        return
      end if
    case ("step_iter")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'step_iter'"
        return
      end if
    case default
      status = NML_ERR_INVALID_NAME
      if (present(errmsg)) errmsg = "unknown field: " // trim(name)
    end select
    if (status == NML_ERR_NOT_SET .and. present(errmsg)) then
      if (len_trim(errmsg) == 0) errmsg = "field not set: " // trim(name)
    end if
  end function nml_config_mrm_is_set

  !> \brief Determine the filled shape along flexible dimensions
  integer function nml_config_mrm_filled_shape(this, name, filled, errmsg) result(status)
    class(nml_config_mrm_t), intent(in) :: this
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
    select case (trim(name))
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
        if (.not. (this%resolution(idx) == -huge(this%resolution(idx)))) then
          filled(1) = idx - lbound(this%resolution, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%resolution, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%resolution(lb_1:ub_1) == -huge(this%resolution(lb_1:ub_1)))) then
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
    case default
      status = NML_ERR_INVALID_NAME
      if (present(errmsg)) errmsg = "field is not a flexible array: " // trim(name)
    end select
  end function nml_config_mrm_filled_shape

  !> \brief Validate required values and constraints
  integer function nml_config_mrm_is_valid(this, errmsg) result(status)
    class(nml_config_mrm_t), intent(in) :: this
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
    if (minval(filled) == 0) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: resolution"
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
  end function nml_config_mrm_is_valid

end module nml_config_mrm
