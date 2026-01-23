!> \file nml_config_meteo.f90
!> \copydoc nml_config_meteo

!> \brief Meteorological configuration
!> \details Configuration for meteorological input data handling in mHM.
!! Meteorological weights can be used to disaggregate daily data to hourly values.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Jan 2026
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_namelists
module nml_config_meteo
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
  use ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_is_nan
  ! kind specifiers listed in the nml-tools configuration file
  use mo_kind, only: &
    dp

  implicit none

  ! default values
  logical, parameter, public :: read_meteo_weights_default = .false.
  character(len=buf), parameter, public :: pre_weights_var_default = "pre_weights"
  character(len=buf), parameter, public :: pet_weights_var_default = "pet_weights"
  character(len=buf), parameter, public :: temp_weights_var_default = "tavg_weights"
  character(len=buf), parameter, public :: ssrd_weights_var_default = "ssrd_weights"
  character(len=buf), parameter, public :: strd_weights_var_default = "strd_weights"
  logical, parameter, public :: share_frac_default = .true.

  !> \class nml_config_meteo_t
  !> \brief Meteorological configuration
  !> \details Configuration for meteorological input data handling in mHM.
  !! Meteorological weights can be used to disaggregate daily data to hourly values.
  type, public :: nml_config_meteo_t
    logical :: is_configured = .false. !< whether the namelist has been configured
    logical, dimension(max_domains) :: read_meteo_weights !< Read meteorological weights
    character(len=buf), dimension(max_domains) :: pre_weights_path !< Precipitation weights path
    character(len=buf), dimension(max_domains) :: pet_weights_path !< Potential evapotranspiration weights path
    character(len=buf), dimension(max_domains) :: temp_weights_path !< Surface downward shortwave radiation weights path
    character(len=buf), dimension(max_domains) :: ssrd_weights_path !< Surface downward shortwave radiation weights path
    character(len=buf), dimension(max_domains) :: strd_weights_path !< Surface downward longwave radiation weights path
    character(len=buf), dimension(max_domains) :: pre_weights_var !< Precipitation weights variable name
    character(len=buf), dimension(max_domains) :: pet_weights_var !< Potential evapotranspiration weights variable name
    character(len=buf), dimension(max_domains) :: temp_weights_var !< Average temperature weights variable name
    character(len=buf), dimension(max_domains) :: ssrd_weights_var !< Surface downward shortwave radiation weights variable name
    character(len=buf), dimension(max_domains) :: strd_weights_var !< Surface downward longwave radiation weights variable name
    real(dp), dimension(12, max_domains) :: frac_night_pre !< Fraction of nightly precipitation
    real(dp), dimension(12, max_domains) :: frac_night_pet !< Fraction of nightly potential evapotranspiration
    real(dp), dimension(12, max_domains) :: frac_night_temp !< Fraction of nightly temperature
    real(dp), dimension(12, max_domains) :: frac_night_ssrd !< Fraction of nightly surface downward shortwave radiation
    real(dp), dimension(12, max_domains) :: frac_night_strd !< Fraction of nightly surface downward longwave radiation
    logical :: share_frac !< Share fractions between domains
  contains
    procedure :: init => nml_config_meteo_init
    procedure :: from_file => nml_config_meteo_from_file
    procedure :: set => nml_config_meteo_set
    procedure :: is_set => nml_config_meteo_is_set
    procedure :: filled_shape => nml_config_meteo_filled_shape
    procedure :: is_valid => nml_config_meteo_is_valid
  end type nml_config_meteo_t

contains

  !> \brief Initialize defaults and sentinels for config_meteo
  integer function nml_config_meteo_init(this, errmsg) result(status)
    class(nml_config_meteo_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    this%is_configured = .false.

    ! sentinel values for required/optional parameters
    this%pre_weights_path = repeat(achar(0), len(this%pre_weights_path)) ! sentinel for optional string array
    this%pet_weights_path = repeat(achar(0), len(this%pet_weights_path)) ! sentinel for optional string array
    this%temp_weights_path = repeat(achar(0), len(this%temp_weights_path)) ! sentinel for optional string array
    this%ssrd_weights_path = repeat(achar(0), len(this%ssrd_weights_path)) ! sentinel for optional string array
    this%strd_weights_path = repeat(achar(0), len(this%strd_weights_path)) ! sentinel for optional string array
    this%frac_night_pre = ieee_value(this%frac_night_pre, ieee_quiet_nan) ! sentinel for optional real array
    this%frac_night_pet = ieee_value(this%frac_night_pet, ieee_quiet_nan) ! sentinel for optional real array
    this%frac_night_temp = ieee_value(this%frac_night_temp, ieee_quiet_nan) ! sentinel for optional real array
    this%frac_night_ssrd = ieee_value(this%frac_night_ssrd, ieee_quiet_nan) ! sentinel for optional real array
    this%frac_night_strd = ieee_value(this%frac_night_strd, ieee_quiet_nan) ! sentinel for optional real array
    ! default values
    this%read_meteo_weights = read_meteo_weights_default
    this%pre_weights_var = pre_weights_var_default
    this%pet_weights_var = pet_weights_var_default
    this%temp_weights_var = temp_weights_var_default
    this%ssrd_weights_var = ssrd_weights_var_default
    this%strd_weights_var = strd_weights_var_default
    this%share_frac = share_frac_default ! bool values always need a default
  end function nml_config_meteo_init

  !> \brief Read config_meteo namelist from file
  integer function nml_config_meteo_from_file(this, file, errmsg) result(status)
    class(nml_config_meteo_t), intent(inout) :: this
    character(len=*), intent(in) :: file !< path to namelist file
    character(len=*), intent(out), optional :: errmsg
    ! namelist variables
    logical, dimension(max_domains) :: read_meteo_weights
    character(len=buf), dimension(max_domains) :: pre_weights_path
    character(len=buf), dimension(max_domains) :: pet_weights_path
    character(len=buf), dimension(max_domains) :: temp_weights_path
    character(len=buf), dimension(max_domains) :: ssrd_weights_path
    character(len=buf), dimension(max_domains) :: strd_weights_path
    character(len=buf), dimension(max_domains) :: pre_weights_var
    character(len=buf), dimension(max_domains) :: pet_weights_var
    character(len=buf), dimension(max_domains) :: temp_weights_var
    character(len=buf), dimension(max_domains) :: ssrd_weights_var
    character(len=buf), dimension(max_domains) :: strd_weights_var
    real(dp), dimension(12, max_domains) :: frac_night_pre
    real(dp), dimension(12, max_domains) :: frac_night_pet
    real(dp), dimension(12, max_domains) :: frac_night_temp
    real(dp), dimension(12, max_domains) :: frac_night_ssrd
    real(dp), dimension(12, max_domains) :: frac_night_strd
    logical :: share_frac
    ! locals
    type(nml_file_t) :: nml
    integer :: iostat
    integer :: close_status
    character(len=nml_line_buffer) :: iomsg

    namelist /config_meteo/ &
      read_meteo_weights, &
      pre_weights_path, &
      pet_weights_path, &
      temp_weights_path, &
      ssrd_weights_path, &
      strd_weights_path, &
      pre_weights_var, &
      pet_weights_var, &
      temp_weights_var, &
      ssrd_weights_var, &
      strd_weights_var, &
      frac_night_pre, &
      frac_night_pet, &
      frac_night_temp, &
      frac_night_ssrd, &
      frac_night_strd, &
      share_frac

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return
    read_meteo_weights = this%read_meteo_weights
    pre_weights_path = this%pre_weights_path
    pet_weights_path = this%pet_weights_path
    temp_weights_path = this%temp_weights_path
    ssrd_weights_path = this%ssrd_weights_path
    strd_weights_path = this%strd_weights_path
    pre_weights_var = this%pre_weights_var
    pet_weights_var = this%pet_weights_var
    temp_weights_var = this%temp_weights_var
    ssrd_weights_var = this%ssrd_weights_var
    strd_weights_var = this%strd_weights_var
    frac_night_pre = this%frac_night_pre
    frac_night_pet = this%frac_night_pet
    frac_night_temp = this%frac_night_temp
    frac_night_ssrd = this%frac_night_ssrd
    frac_night_strd = this%frac_night_strd
    share_frac = this%share_frac

    status = nml%open(file, errmsg=errmsg)
    if (status /= NML_OK) return

    status = nml%find("config_meteo", errmsg=errmsg)
    if (status /= NML_OK) then
      close_status = nml%close()
      return
    end if

    ! read namelist
    read(nml%unit, nml=config_meteo, iostat=iostat, iomsg=iomsg)
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
    this%read_meteo_weights = read_meteo_weights
    this%pre_weights_path = pre_weights_path
    this%pet_weights_path = pet_weights_path
    this%temp_weights_path = temp_weights_path
    this%ssrd_weights_path = ssrd_weights_path
    this%strd_weights_path = strd_weights_path
    this%pre_weights_var = pre_weights_var
    this%pet_weights_var = pet_weights_var
    this%temp_weights_var = temp_weights_var
    this%ssrd_weights_var = ssrd_weights_var
    this%strd_weights_var = strd_weights_var
    this%frac_night_pre = frac_night_pre
    this%frac_night_pet = frac_night_pet
    this%frac_night_temp = frac_night_temp
    this%frac_night_ssrd = frac_night_ssrd
    this%frac_night_strd = frac_night_strd
    this%share_frac = share_frac

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_config_meteo_from_file

  !> \brief Set config_meteo values
  integer function nml_config_meteo_set(this, &
    read_meteo_weights, &
    pre_weights_path, &
    pet_weights_path, &
    temp_weights_path, &
    ssrd_weights_path, &
    strd_weights_path, &
    pre_weights_var, &
    pet_weights_var, &
    temp_weights_var, &
    ssrd_weights_var, &
    strd_weights_var, &
    frac_night_pre, &
    frac_night_pet, &
    frac_night_temp, &
    frac_night_ssrd, &
    frac_night_strd, &
    share_frac, &
    errmsg) result(status)

    class(nml_config_meteo_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg
    logical, dimension(:), intent(in), optional :: read_meteo_weights
    character(len=*), dimension(:), intent(in), optional :: pre_weights_path
    character(len=*), dimension(:), intent(in), optional :: pet_weights_path
    character(len=*), dimension(:), intent(in), optional :: temp_weights_path
    character(len=*), dimension(:), intent(in), optional :: ssrd_weights_path
    character(len=*), dimension(:), intent(in), optional :: strd_weights_path
    character(len=*), dimension(:), intent(in), optional :: pre_weights_var
    character(len=*), dimension(:), intent(in), optional :: pet_weights_var
    character(len=*), dimension(:), intent(in), optional :: temp_weights_var
    character(len=*), dimension(:), intent(in), optional :: ssrd_weights_var
    character(len=*), dimension(:), intent(in), optional :: strd_weights_var
    real(dp), dimension(:, :), intent(in), optional :: frac_night_pre
    real(dp), dimension(:, :), intent(in), optional :: frac_night_pet
    real(dp), dimension(:, :), intent(in), optional :: frac_night_temp
    real(dp), dimension(:, :), intent(in), optional :: frac_night_ssrd
    real(dp), dimension(:, :), intent(in), optional :: frac_night_strd
    logical, intent(in), optional :: share_frac
    integer :: &
      lb_1, &
      lb_2, &
      ub_1, &
      ub_2

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return

    ! required parameters
    ! override with provided values
    if (present(read_meteo_weights)) then
      if (size(read_meteo_weights, 1) > size(this%read_meteo_weights, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'read_meteo_weights'"
        return
      end if
      lb_1 = lbound(this%read_meteo_weights, 1)
      ub_1 = lb_1 + size(read_meteo_weights, 1) - 1
      this%read_meteo_weights(lb_1:ub_1) = read_meteo_weights
    end if
    if (present(pre_weights_path)) then
      if (size(pre_weights_path, 1) > size(this%pre_weights_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'pre_weights_path'"
        return
      end if
      lb_1 = lbound(this%pre_weights_path, 1)
      ub_1 = lb_1 + size(pre_weights_path, 1) - 1
      this%pre_weights_path(lb_1:ub_1) = pre_weights_path
    end if
    if (present(pet_weights_path)) then
      if (size(pet_weights_path, 1) > size(this%pet_weights_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'pet_weights_path'"
        return
      end if
      lb_1 = lbound(this%pet_weights_path, 1)
      ub_1 = lb_1 + size(pet_weights_path, 1) - 1
      this%pet_weights_path(lb_1:ub_1) = pet_weights_path
    end if
    if (present(temp_weights_path)) then
      if (size(temp_weights_path, 1) > size(this%temp_weights_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'temp_weights_path'"
        return
      end if
      lb_1 = lbound(this%temp_weights_path, 1)
      ub_1 = lb_1 + size(temp_weights_path, 1) - 1
      this%temp_weights_path(lb_1:ub_1) = temp_weights_path
    end if
    if (present(ssrd_weights_path)) then
      if (size(ssrd_weights_path, 1) > size(this%ssrd_weights_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'ssrd_weights_path'"
        return
      end if
      lb_1 = lbound(this%ssrd_weights_path, 1)
      ub_1 = lb_1 + size(ssrd_weights_path, 1) - 1
      this%ssrd_weights_path(lb_1:ub_1) = ssrd_weights_path
    end if
    if (present(strd_weights_path)) then
      if (size(strd_weights_path, 1) > size(this%strd_weights_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'strd_weights_path'"
        return
      end if
      lb_1 = lbound(this%strd_weights_path, 1)
      ub_1 = lb_1 + size(strd_weights_path, 1) - 1
      this%strd_weights_path(lb_1:ub_1) = strd_weights_path
    end if
    if (present(pre_weights_var)) then
      if (size(pre_weights_var, 1) > size(this%pre_weights_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'pre_weights_var'"
        return
      end if
      lb_1 = lbound(this%pre_weights_var, 1)
      ub_1 = lb_1 + size(pre_weights_var, 1) - 1
      this%pre_weights_var(lb_1:ub_1) = pre_weights_var
    end if
    if (present(pet_weights_var)) then
      if (size(pet_weights_var, 1) > size(this%pet_weights_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'pet_weights_var'"
        return
      end if
      lb_1 = lbound(this%pet_weights_var, 1)
      ub_1 = lb_1 + size(pet_weights_var, 1) - 1
      this%pet_weights_var(lb_1:ub_1) = pet_weights_var
    end if
    if (present(temp_weights_var)) then
      if (size(temp_weights_var, 1) > size(this%temp_weights_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'temp_weights_var'"
        return
      end if
      lb_1 = lbound(this%temp_weights_var, 1)
      ub_1 = lb_1 + size(temp_weights_var, 1) - 1
      this%temp_weights_var(lb_1:ub_1) = temp_weights_var
    end if
    if (present(ssrd_weights_var)) then
      if (size(ssrd_weights_var, 1) > size(this%ssrd_weights_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'ssrd_weights_var'"
        return
      end if
      lb_1 = lbound(this%ssrd_weights_var, 1)
      ub_1 = lb_1 + size(ssrd_weights_var, 1) - 1
      this%ssrd_weights_var(lb_1:ub_1) = ssrd_weights_var
    end if
    if (present(strd_weights_var)) then
      if (size(strd_weights_var, 1) > size(this%strd_weights_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'strd_weights_var'"
        return
      end if
      lb_1 = lbound(this%strd_weights_var, 1)
      ub_1 = lb_1 + size(strd_weights_var, 1) - 1
      this%strd_weights_var(lb_1:ub_1) = strd_weights_var
    end if
    if (present(frac_night_pre)) then
      if (size(frac_night_pre, 1) /= size(this%frac_night_pre, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 mismatch for 'frac_night_pre'"
        return
      end if
      if (size(frac_night_pre, 2) > size(this%frac_night_pre, 2)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 2 exceeds bounds for 'frac_night_pre'"
        return
      end if
      lb_2 = lbound(this%frac_night_pre, 2)
      ub_2 = lb_2 + size(frac_night_pre, 2) - 1
      this%frac_night_pre(:, lb_2:ub_2) = frac_night_pre
    end if
    if (present(frac_night_pet)) then
      if (size(frac_night_pet, 1) /= size(this%frac_night_pet, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 mismatch for 'frac_night_pet'"
        return
      end if
      if (size(frac_night_pet, 2) > size(this%frac_night_pet, 2)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 2 exceeds bounds for 'frac_night_pet'"
        return
      end if
      lb_2 = lbound(this%frac_night_pet, 2)
      ub_2 = lb_2 + size(frac_night_pet, 2) - 1
      this%frac_night_pet(:, lb_2:ub_2) = frac_night_pet
    end if
    if (present(frac_night_temp)) then
      if (size(frac_night_temp, 1) /= size(this%frac_night_temp, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 mismatch for 'frac_night_temp'"
        return
      end if
      if (size(frac_night_temp, 2) > size(this%frac_night_temp, 2)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 2 exceeds bounds for 'frac_night_temp'"
        return
      end if
      lb_2 = lbound(this%frac_night_temp, 2)
      ub_2 = lb_2 + size(frac_night_temp, 2) - 1
      this%frac_night_temp(:, lb_2:ub_2) = frac_night_temp
    end if
    if (present(frac_night_ssrd)) then
      if (size(frac_night_ssrd, 1) /= size(this%frac_night_ssrd, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 mismatch for 'frac_night_ssrd'"
        return
      end if
      if (size(frac_night_ssrd, 2) > size(this%frac_night_ssrd, 2)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 2 exceeds bounds for 'frac_night_ssrd'"
        return
      end if
      lb_2 = lbound(this%frac_night_ssrd, 2)
      ub_2 = lb_2 + size(frac_night_ssrd, 2) - 1
      this%frac_night_ssrd(:, lb_2:ub_2) = frac_night_ssrd
    end if
    if (present(frac_night_strd)) then
      if (size(frac_night_strd, 1) /= size(this%frac_night_strd, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 mismatch for 'frac_night_strd'"
        return
      end if
      if (size(frac_night_strd, 2) > size(this%frac_night_strd, 2)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 2 exceeds bounds for 'frac_night_strd'"
        return
      end if
      lb_2 = lbound(this%frac_night_strd, 2)
      ub_2 = lb_2 + size(frac_night_strd, 2) - 1
      this%frac_night_strd(:, lb_2:ub_2) = frac_night_strd
    end if
    if (present(share_frac)) this%share_frac = share_frac

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_config_meteo_set

  !> \brief Check whether a namelist value was set
  integer function nml_config_meteo_is_set(this, name, idx, errmsg) result(status)
    class(nml_config_meteo_t), intent(in) :: this
    character(len=*), intent(in) :: name
    integer, intent(in), optional :: idx(:)
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    select case (trim(name))
    case ("read_meteo_weights")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%read_meteo_weights), ubound(this%read_meteo_weights), &
          "read_meteo_weights", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("pre_weights_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%pre_weights_path), ubound(this%pre_weights_path), &
          "pre_weights_path", errmsg)
        if (status /= NML_OK) return
        if (this%pre_weights_path(idx(1)) == repeat(achar(0), len(this%pre_weights_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%pre_weights_path == repeat(achar(0), len(this%pre_weights_path)))) status = NML_ERR_NOT_SET
      end if
    case ("pet_weights_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%pet_weights_path), ubound(this%pet_weights_path), &
          "pet_weights_path", errmsg)
        if (status /= NML_OK) return
        if (this%pet_weights_path(idx(1)) == repeat(achar(0), len(this%pet_weights_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%pet_weights_path == repeat(achar(0), len(this%pet_weights_path)))) status = NML_ERR_NOT_SET
      end if
    case ("temp_weights_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%temp_weights_path), ubound(this%temp_weights_path), &
          "temp_weights_path", errmsg)
        if (status /= NML_OK) return
        if (this%temp_weights_path(idx(1)) == repeat(achar(0), len(this%temp_weights_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%temp_weights_path == repeat(achar(0), len(this%temp_weights_path)))) status = NML_ERR_NOT_SET
      end if
    case ("ssrd_weights_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%ssrd_weights_path), ubound(this%ssrd_weights_path), &
          "ssrd_weights_path", errmsg)
        if (status /= NML_OK) return
        if (this%ssrd_weights_path(idx(1)) == repeat(achar(0), len(this%ssrd_weights_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%ssrd_weights_path == repeat(achar(0), len(this%ssrd_weights_path)))) status = NML_ERR_NOT_SET
      end if
    case ("strd_weights_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%strd_weights_path), ubound(this%strd_weights_path), &
          "strd_weights_path", errmsg)
        if (status /= NML_OK) return
        if (this%strd_weights_path(idx(1)) == repeat(achar(0), len(this%strd_weights_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%strd_weights_path == repeat(achar(0), len(this%strd_weights_path)))) status = NML_ERR_NOT_SET
      end if
    case ("pre_weights_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%pre_weights_var), ubound(this%pre_weights_var), &
          "pre_weights_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("pet_weights_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%pet_weights_var), ubound(this%pet_weights_var), &
          "pet_weights_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("temp_weights_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%temp_weights_var), ubound(this%temp_weights_var), &
          "temp_weights_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("ssrd_weights_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%ssrd_weights_var), ubound(this%ssrd_weights_var), &
          "ssrd_weights_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("strd_weights_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%strd_weights_var), ubound(this%strd_weights_var), &
          "strd_weights_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("frac_night_pre")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%frac_night_pre), ubound(this%frac_night_pre), &
          "frac_night_pre", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%frac_night_pre(idx(1), idx(2)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%frac_night_pre))) status = NML_ERR_NOT_SET
      end if
    case ("frac_night_pet")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%frac_night_pet), ubound(this%frac_night_pet), &
          "frac_night_pet", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%frac_night_pet(idx(1), idx(2)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%frac_night_pet))) status = NML_ERR_NOT_SET
      end if
    case ("frac_night_temp")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%frac_night_temp), ubound(this%frac_night_temp), &
          "frac_night_temp", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%frac_night_temp(idx(1), idx(2)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%frac_night_temp))) status = NML_ERR_NOT_SET
      end if
    case ("frac_night_ssrd")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%frac_night_ssrd), ubound(this%frac_night_ssrd), &
          "frac_night_ssrd", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%frac_night_ssrd(idx(1), idx(2)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%frac_night_ssrd))) status = NML_ERR_NOT_SET
      end if
    case ("frac_night_strd")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%frac_night_strd), ubound(this%frac_night_strd), &
          "frac_night_strd", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%frac_night_strd(idx(1), idx(2)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%frac_night_strd))) status = NML_ERR_NOT_SET
      end if
    case ("share_frac")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'share_frac'"
        return
      end if
    case default
      status = NML_ERR_INVALID_NAME
      if (present(errmsg)) errmsg = "unknown field: " // trim(name)
    end select
    if (status == NML_ERR_NOT_SET .and. present(errmsg)) then
      if (len_trim(errmsg) == 0) errmsg = "field not set: " // trim(name)
    end if
  end function nml_config_meteo_is_set

  !> \brief Determine the filled shape along flexible dimensions
  integer function nml_config_meteo_filled_shape(this, name, filled, errmsg) result(status)
    class(nml_config_meteo_t), intent(in) :: this
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
    select case (trim(name))
    case ("pre_weights_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'pre_weights_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%pre_weights_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%pre_weights_path, 1), &
        lbound(this%pre_weights_path, 1), -1
        if (.not. (this%pre_weights_path(idx) == repeat(achar(0), len(this%pre_weights_path)))) then
          filled(1) = idx - lbound(this%pre_weights_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%pre_weights_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%pre_weights_path(lb_1:ub_1) == repeat(achar(0), len(this%pre_weights_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: pre_weights_path"
          return
        end if
      end if
    case ("pet_weights_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'pet_weights_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%pet_weights_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%pet_weights_path, 1), &
        lbound(this%pet_weights_path, 1), -1
        if (.not. (this%pet_weights_path(idx) == repeat(achar(0), len(this%pet_weights_path)))) then
          filled(1) = idx - lbound(this%pet_weights_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%pet_weights_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%pet_weights_path(lb_1:ub_1) == repeat(achar(0), len(this%pet_weights_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: pet_weights_path"
          return
        end if
      end if
    case ("temp_weights_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'temp_weights_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%temp_weights_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%temp_weights_path, 1), &
        lbound(this%temp_weights_path, 1), -1
        if (.not. (this%temp_weights_path(idx) == repeat(achar(0), len(this%temp_weights_path)))) then
          filled(1) = idx - lbound(this%temp_weights_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%temp_weights_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%temp_weights_path(lb_1:ub_1) == repeat(achar(0), len(this%temp_weights_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: temp_weights_path"
          return
        end if
      end if
    case ("ssrd_weights_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'ssrd_weights_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%ssrd_weights_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%ssrd_weights_path, 1), &
        lbound(this%ssrd_weights_path, 1), -1
        if (.not. (this%ssrd_weights_path(idx) == repeat(achar(0), len(this%ssrd_weights_path)))) then
          filled(1) = idx - lbound(this%ssrd_weights_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%ssrd_weights_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%ssrd_weights_path(lb_1:ub_1) == repeat(achar(0), len(this%ssrd_weights_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: ssrd_weights_path"
          return
        end if
      end if
    case ("strd_weights_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'strd_weights_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%strd_weights_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%strd_weights_path, 1), &
        lbound(this%strd_weights_path, 1), -1
        if (.not. (this%strd_weights_path(idx) == repeat(achar(0), len(this%strd_weights_path)))) then
          filled(1) = idx - lbound(this%strd_weights_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%strd_weights_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%strd_weights_path(lb_1:ub_1) == repeat(achar(0), len(this%strd_weights_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: strd_weights_path"
          return
        end if
      end if
    case ("frac_night_pre")
      if (size(filled) /= 2) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'frac_night_pre'"
        return
      end if
      do dim = 1, 2
        filled(dim) = size(this%frac_night_pre, dim)
      end do
      filled(2) = 0
      do idx = ubound(this%frac_night_pre, 2), &
        lbound(this%frac_night_pre, 2), -1
        if (.not. (all(ieee_is_nan(this%frac_night_pre(:, idx))))) then
          filled(2) = idx - lbound(this%frac_night_pre, 2) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_2 = lbound(this%frac_night_pre, 2)
        ub_2 = lb_2 + filled(2) - 1
        if (any(ieee_is_nan(this%frac_night_pre(:, lb_2:ub_2)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: frac_night_pre"
          return
        end if
      end if
    case ("frac_night_pet")
      if (size(filled) /= 2) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'frac_night_pet'"
        return
      end if
      do dim = 1, 2
        filled(dim) = size(this%frac_night_pet, dim)
      end do
      filled(2) = 0
      do idx = ubound(this%frac_night_pet, 2), &
        lbound(this%frac_night_pet, 2), -1
        if (.not. (all(ieee_is_nan(this%frac_night_pet(:, idx))))) then
          filled(2) = idx - lbound(this%frac_night_pet, 2) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_2 = lbound(this%frac_night_pet, 2)
        ub_2 = lb_2 + filled(2) - 1
        if (any(ieee_is_nan(this%frac_night_pet(:, lb_2:ub_2)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: frac_night_pet"
          return
        end if
      end if
    case ("frac_night_temp")
      if (size(filled) /= 2) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'frac_night_temp'"
        return
      end if
      do dim = 1, 2
        filled(dim) = size(this%frac_night_temp, dim)
      end do
      filled(2) = 0
      do idx = ubound(this%frac_night_temp, 2), &
        lbound(this%frac_night_temp, 2), -1
        if (.not. (all(ieee_is_nan(this%frac_night_temp(:, idx))))) then
          filled(2) = idx - lbound(this%frac_night_temp, 2) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_2 = lbound(this%frac_night_temp, 2)
        ub_2 = lb_2 + filled(2) - 1
        if (any(ieee_is_nan(this%frac_night_temp(:, lb_2:ub_2)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: frac_night_temp"
          return
        end if
      end if
    case ("frac_night_ssrd")
      if (size(filled) /= 2) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'frac_night_ssrd'"
        return
      end if
      do dim = 1, 2
        filled(dim) = size(this%frac_night_ssrd, dim)
      end do
      filled(2) = 0
      do idx = ubound(this%frac_night_ssrd, 2), &
        lbound(this%frac_night_ssrd, 2), -1
        if (.not. (all(ieee_is_nan(this%frac_night_ssrd(:, idx))))) then
          filled(2) = idx - lbound(this%frac_night_ssrd, 2) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_2 = lbound(this%frac_night_ssrd, 2)
        ub_2 = lb_2 + filled(2) - 1
        if (any(ieee_is_nan(this%frac_night_ssrd(:, lb_2:ub_2)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: frac_night_ssrd"
          return
        end if
      end if
    case ("frac_night_strd")
      if (size(filled) /= 2) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'frac_night_strd'"
        return
      end if
      do dim = 1, 2
        filled(dim) = size(this%frac_night_strd, dim)
      end do
      filled(2) = 0
      do idx = ubound(this%frac_night_strd, 2), &
        lbound(this%frac_night_strd, 2), -1
        if (.not. (all(ieee_is_nan(this%frac_night_strd(:, idx))))) then
          filled(2) = idx - lbound(this%frac_night_strd, 2) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_2 = lbound(this%frac_night_strd, 2)
        ub_2 = lb_2 + filled(2) - 1
        if (any(ieee_is_nan(this%frac_night_strd(:, lb_2:ub_2)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: frac_night_strd"
          return
        end if
      end if
    case default
      status = NML_ERR_INVALID_NAME
      if (present(errmsg)) errmsg = "field is not a flexible array: " // trim(name)
    end select
  end function nml_config_meteo_filled_shape

  !> \brief Validate required values and constraints
  integer function nml_config_meteo_is_valid(this, errmsg) result(status)
    class(nml_config_meteo_t), intent(in) :: this
    character(len=*), intent(out), optional :: errmsg
    integer :: istat
    integer, allocatable :: filled(:)

    status = NML_OK
    if (present(errmsg)) errmsg = ""

    ! flexible arrays
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("pre_weights_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: pre_weights_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("pet_weights_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: pet_weights_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("temp_weights_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: temp_weights_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("ssrd_weights_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: ssrd_weights_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("strd_weights_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: strd_weights_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(2))
    istat = this%filled_shape("frac_night_pre", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: frac_night_pre"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(2))
    istat = this%filled_shape("frac_night_pet", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: frac_night_pet"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(2))
    istat = this%filled_shape("frac_night_temp", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: frac_night_temp"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(2))
    istat = this%filled_shape("frac_night_ssrd", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: frac_night_ssrd"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(2))
    istat = this%filled_shape("frac_night_strd", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: frac_night_strd"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
  end function nml_config_meteo_is_valid

end module nml_config_meteo
