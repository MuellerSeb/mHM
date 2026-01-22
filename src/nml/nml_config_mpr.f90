!> \file nml_config_mpr.f90
!> \copydoc nml_config_mpr

!> \brief MPR configuration
!> \details Configuration for the multiscale parameter regionalization in mHM.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Jan 2026
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_namelists
module nml_config_mpr
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
    max_layers, &
    buf, &
    NML_ERR_PARTLY_SET
  use ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_is_nan
  ! kind specifiers listed in the nml-tools configuration file
  use mo_kind, only: &
    i4, &
    dp

  implicit none

  ! default values
  integer(i4), parameter, public :: soil_db_mode_default = 0_i4

  ! enum values
  integer(i4), parameter, public :: soil_db_mode_enum_values(2) = [0_i4, 1_i4]
  integer(i4), parameter, public :: lai_time_step_enum_values(5) = [-3_i4, -2_i4, -1_i4, 0_i4, 1_i4]

  !> \class nml_config_mpr_t
  !> \brief MPR configuration
  !> \details Configuration for the multiscale parameter regionalization in mHM.
  type, public :: nml_config_mpr_t
    logical :: is_configured = .false. !< whether the namelist has been configured
    integer(i4), dimension(max_domains) :: soil_db_mode !< Soil database mode
    integer(i4), dimension(max_domains) :: n_horizons !< Number of soil horizons
    integer(i4), dimension(max_domains) :: tillage_depth !< Tillage depth
    integer(i4), dimension(max_layers, max_domains) :: soil_depth !< Soil horizon depth
    real(dp), dimension(max_domains) :: fracSealed_cityArea !< Sealed fraction of city area
    character(len=buf), dimension(max_domains) :: land_cover_path !< Land cover path
    integer(i4), dimension(max_domains) :: lai_time_step !< LAI time step
    character(len=buf), dimension(max_domains) :: lai_path !< LAI path
    character(len=buf), dimension(max_domains) :: soil_lut_path !< Soil LUT path
    character(len=buf), dimension(max_domains) :: geo_lut_path !< Geology LUT path
    character(len=buf), dimension(max_domains) :: lai_lut_path !< LAI LUT path
  contains
    procedure :: init => nml_config_mpr_init
    procedure :: from_file => nml_config_mpr_from_file
    procedure :: set => nml_config_mpr_set
    procedure :: is_set => nml_config_mpr_is_set
    procedure :: filled_shape => nml_config_mpr_filled_shape
    procedure :: is_valid => nml_config_mpr_is_valid
  end type nml_config_mpr_t

contains

  !> \brief Check whether a value is part of an enum
  elemental logical function soil_db_mode_in_enum(val, allow_missing) result(in_enum)
    integer(i4), intent(in) :: val
    logical, intent(in), optional :: allow_missing

    if (present(allow_missing)) then
      if (allow_missing) then
        if (val == -huge(val)) then
          in_enum = .true.
          return
        end if
      end if
    end if
    in_enum = any(val == soil_db_mode_enum_values)
  end function soil_db_mode_in_enum

  !> \brief Check whether a value is part of an enum
  elemental logical function lai_time_step_in_enum(val, allow_missing) result(in_enum)
    integer(i4), intent(in) :: val
    logical, intent(in), optional :: allow_missing

    if (present(allow_missing)) then
      if (allow_missing) then
        if (val == -huge(val)) then
          in_enum = .true.
          return
        end if
      end if
    end if
    in_enum = any(val == lai_time_step_enum_values)
  end function lai_time_step_in_enum

  !> \brief Initialize defaults and sentinels for config_mpr
  integer function nml_config_mpr_init(this, errmsg) result(status)
    class(nml_config_mpr_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    this%is_configured = .false.

    ! sentinel values for required/optional parameters
    this%n_horizons = -huge(this%n_horizons) ! sentinel for optional integer array
    this%tillage_depth = -huge(this%tillage_depth) ! sentinel for optional integer array
    this%soil_depth = -huge(this%soil_depth) ! sentinel for optional integer array
    this%fracSealed_cityArea = ieee_value(this%fracSealed_cityArea, ieee_quiet_nan) ! sentinel for optional real array
    this%land_cover_path = repeat(achar(0), len(this%land_cover_path)) ! sentinel for optional string array
    this%lai_time_step = -huge(this%lai_time_step) ! sentinel for optional integer array
    this%lai_path = repeat(achar(0), len(this%lai_path)) ! sentinel for optional string array
    this%soil_lut_path = repeat(achar(0), len(this%soil_lut_path)) ! sentinel for optional string array
    this%geo_lut_path = repeat(achar(0), len(this%geo_lut_path)) ! sentinel for optional string array
    this%lai_lut_path = repeat(achar(0), len(this%lai_lut_path)) ! sentinel for optional string array
    ! default values
    this%soil_db_mode = soil_db_mode_default
  end function nml_config_mpr_init

  !> \brief Read config_mpr namelist from file
  integer function nml_config_mpr_from_file(this, file, errmsg) result(status)
    class(nml_config_mpr_t), intent(inout) :: this
    character(len=*), intent(in) :: file !< path to namelist file
    character(len=*), intent(out), optional :: errmsg
    ! namelist variables
    integer(i4), dimension(max_domains) :: soil_db_mode
    integer(i4), dimension(max_domains) :: n_horizons
    integer(i4), dimension(max_domains) :: tillage_depth
    integer(i4), dimension(max_layers, max_domains) :: soil_depth
    real(dp), dimension(max_domains) :: fracSealed_cityArea
    character(len=buf), dimension(max_domains) :: land_cover_path
    integer(i4), dimension(max_domains) :: lai_time_step
    character(len=buf), dimension(max_domains) :: lai_path
    character(len=buf), dimension(max_domains) :: soil_lut_path
    character(len=buf), dimension(max_domains) :: geo_lut_path
    character(len=buf), dimension(max_domains) :: lai_lut_path
    ! locals
    type(nml_file_t) :: nml
    integer :: iostat
    integer :: close_status
    character(len=nml_line_buffer) :: iomsg

    namelist /config_mpr/ &
      soil_db_mode, &
      n_horizons, &
      tillage_depth, &
      soil_depth, &
      fracSealed_cityArea, &
      land_cover_path, &
      lai_time_step, &
      lai_path, &
      soil_lut_path, &
      geo_lut_path, &
      lai_lut_path

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return
    soil_db_mode = this%soil_db_mode
    n_horizons = this%n_horizons
    tillage_depth = this%tillage_depth
    soil_depth = this%soil_depth
    fracSealed_cityArea = this%fracSealed_cityArea
    land_cover_path = this%land_cover_path
    lai_time_step = this%lai_time_step
    lai_path = this%lai_path
    soil_lut_path = this%soil_lut_path
    geo_lut_path = this%geo_lut_path
    lai_lut_path = this%lai_lut_path

    status = nml%open(file, errmsg=errmsg)
    if (status /= NML_OK) return

    status = nml%find("config_mpr", errmsg=errmsg)
    if (status /= NML_OK) then
      close_status = nml%close()
      return
    end if

    ! read namelist
    read(nml%unit, nml=config_mpr, iostat=iostat, iomsg=iomsg)
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
    this%soil_db_mode = soil_db_mode
    this%n_horizons = n_horizons
    this%tillage_depth = tillage_depth
    this%soil_depth = soil_depth
    this%fracSealed_cityArea = fracSealed_cityArea
    this%land_cover_path = land_cover_path
    this%lai_time_step = lai_time_step
    this%lai_path = lai_path
    this%soil_lut_path = soil_lut_path
    this%geo_lut_path = geo_lut_path
    this%lai_lut_path = lai_lut_path

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_config_mpr_from_file

  !> \brief Set config_mpr values
  integer function nml_config_mpr_set(this, &
    soil_db_mode, &
    n_horizons, &
    tillage_depth, &
    soil_depth, &
    fracSealed_cityArea, &
    land_cover_path, &
    lai_time_step, &
    lai_path, &
    soil_lut_path, &
    geo_lut_path, &
    lai_lut_path, &
    errmsg) result(status)

    class(nml_config_mpr_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg
    integer(i4), dimension(:), intent(in), optional :: soil_db_mode
    integer(i4), dimension(:), intent(in), optional :: n_horizons
    integer(i4), dimension(:), intent(in), optional :: tillage_depth
    integer(i4), dimension(:, :), intent(in), optional :: soil_depth
    real(dp), dimension(:), intent(in), optional :: fracSealed_cityArea
    character(len=*), dimension(:), intent(in), optional :: land_cover_path
    integer(i4), dimension(:), intent(in), optional :: lai_time_step
    character(len=*), dimension(:), intent(in), optional :: lai_path
    character(len=*), dimension(:), intent(in), optional :: soil_lut_path
    character(len=*), dimension(:), intent(in), optional :: geo_lut_path
    character(len=*), dimension(:), intent(in), optional :: lai_lut_path
    integer :: &
      lb_1, &
      lb_2, &
      ub_1, &
      ub_2

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return

    ! required parameters
    ! override with provided values
    if (present(soil_db_mode)) then
      if (size(soil_db_mode, 1) > size(this%soil_db_mode, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'soil_db_mode'"
        return
      end if
      lb_1 = lbound(this%soil_db_mode, 1)
      ub_1 = lb_1 + size(soil_db_mode, 1) - 1
      this%soil_db_mode(lb_1:ub_1) = soil_db_mode
    end if
    if (present(n_horizons)) then
      if (size(n_horizons, 1) > size(this%n_horizons, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'n_horizons'"
        return
      end if
      lb_1 = lbound(this%n_horizons, 1)
      ub_1 = lb_1 + size(n_horizons, 1) - 1
      this%n_horizons(lb_1:ub_1) = n_horizons
    end if
    if (present(tillage_depth)) then
      if (size(tillage_depth, 1) > size(this%tillage_depth, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'tillage_depth'"
        return
      end if
      lb_1 = lbound(this%tillage_depth, 1)
      ub_1 = lb_1 + size(tillage_depth, 1) - 1
      this%tillage_depth(lb_1:ub_1) = tillage_depth
    end if
    if (present(soil_depth)) then
      if (size(soil_depth, 1) > size(this%soil_depth, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'soil_depth'"
        return
      end if
      lb_1 = lbound(this%soil_depth, 1)
      ub_1 = lb_1 + size(soil_depth, 1) - 1
      if (size(soil_depth, 2) > size(this%soil_depth, 2)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 2 exceeds bounds for 'soil_depth'"
        return
      end if
      lb_2 = lbound(this%soil_depth, 2)
      ub_2 = lb_2 + size(soil_depth, 2) - 1
      this%soil_depth(lb_1:ub_1, lb_2:ub_2) = soil_depth
    end if
    if (present(fracSealed_cityArea)) then
      if (size(fracSealed_cityArea, 1) > size(this%fracSealed_cityArea, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'fracSealed_cityArea'"
        return
      end if
      lb_1 = lbound(this%fracSealed_cityArea, 1)
      ub_1 = lb_1 + size(fracSealed_cityArea, 1) - 1
      this%fracSealed_cityArea(lb_1:ub_1) = fracSealed_cityArea
    end if
    if (present(land_cover_path)) then
      if (size(land_cover_path, 1) > size(this%land_cover_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'land_cover_path'"
        return
      end if
      lb_1 = lbound(this%land_cover_path, 1)
      ub_1 = lb_1 + size(land_cover_path, 1) - 1
      this%land_cover_path(lb_1:ub_1) = land_cover_path
    end if
    if (present(lai_time_step)) then
      if (size(lai_time_step, 1) > size(this%lai_time_step, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'lai_time_step'"
        return
      end if
      lb_1 = lbound(this%lai_time_step, 1)
      ub_1 = lb_1 + size(lai_time_step, 1) - 1
      this%lai_time_step(lb_1:ub_1) = lai_time_step
    end if
    if (present(lai_path)) then
      if (size(lai_path, 1) > size(this%lai_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'lai_path'"
        return
      end if
      lb_1 = lbound(this%lai_path, 1)
      ub_1 = lb_1 + size(lai_path, 1) - 1
      this%lai_path(lb_1:ub_1) = lai_path
    end if
    if (present(soil_lut_path)) then
      if (size(soil_lut_path, 1) > size(this%soil_lut_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'soil_lut_path'"
        return
      end if
      lb_1 = lbound(this%soil_lut_path, 1)
      ub_1 = lb_1 + size(soil_lut_path, 1) - 1
      this%soil_lut_path(lb_1:ub_1) = soil_lut_path
    end if
    if (present(geo_lut_path)) then
      if (size(geo_lut_path, 1) > size(this%geo_lut_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'geo_lut_path'"
        return
      end if
      lb_1 = lbound(this%geo_lut_path, 1)
      ub_1 = lb_1 + size(geo_lut_path, 1) - 1
      this%geo_lut_path(lb_1:ub_1) = geo_lut_path
    end if
    if (present(lai_lut_path)) then
      if (size(lai_lut_path, 1) > size(this%lai_lut_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'lai_lut_path'"
        return
      end if
      lb_1 = lbound(this%lai_lut_path, 1)
      ub_1 = lb_1 + size(lai_lut_path, 1) - 1
      this%lai_lut_path(lb_1:ub_1) = lai_lut_path
    end if

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_config_mpr_set

  !> \brief Check whether a namelist value was set
  integer function nml_config_mpr_is_set(this, name, idx, errmsg) result(status)
    class(nml_config_mpr_t), intent(in) :: this
    character(len=*), intent(in) :: name
    integer, intent(in), optional :: idx(:)
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    select case (trim(name))
    case ("soil_db_mode")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%soil_db_mode), ubound(this%soil_db_mode), &
          "soil_db_mode", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("n_horizons")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%n_horizons), ubound(this%n_horizons), &
          "n_horizons", errmsg)
        if (status /= NML_OK) return
        if (this%n_horizons(idx(1)) == -huge(this%n_horizons(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(this%n_horizons == -huge(this%n_horizons))) status = NML_ERR_NOT_SET
      end if
    case ("tillage_depth")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%tillage_depth), ubound(this%tillage_depth), &
          "tillage_depth", errmsg)
        if (status /= NML_OK) return
        if (this%tillage_depth(idx(1)) == -huge(this%tillage_depth(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(this%tillage_depth == -huge(this%tillage_depth))) status = NML_ERR_NOT_SET
      end if
    case ("soil_depth")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%soil_depth), ubound(this%soil_depth), &
          "soil_depth", errmsg)
        if (status /= NML_OK) return
        if (this%soil_depth(idx(1), idx(2)) == -huge(this%soil_depth(idx(1), idx(2)))) status = NML_ERR_NOT_SET
      else
        if (all(this%soil_depth == -huge(this%soil_depth))) status = NML_ERR_NOT_SET
      end if
    case ("fracSealed_cityArea")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%fracSealed_cityArea), ubound(this%fracSealed_cityArea), &
          "fracSealed_cityArea", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%fracSealed_cityArea(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%fracSealed_cityArea))) status = NML_ERR_NOT_SET
      end if
    case ("land_cover_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%land_cover_path), ubound(this%land_cover_path), &
          "land_cover_path", errmsg)
        if (status /= NML_OK) return
        if (this%land_cover_path(idx(1)) == repeat(achar(0), len(this%land_cover_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%land_cover_path == repeat(achar(0), len(this%land_cover_path)))) status = NML_ERR_NOT_SET
      end if
    case ("lai_time_step")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%lai_time_step), ubound(this%lai_time_step), &
          "lai_time_step", errmsg)
        if (status /= NML_OK) return
        if (this%lai_time_step(idx(1)) == -huge(this%lai_time_step(idx(1)))) status = NML_ERR_NOT_SET
      else
        if (all(this%lai_time_step == -huge(this%lai_time_step))) status = NML_ERR_NOT_SET
      end if
    case ("lai_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%lai_path), ubound(this%lai_path), &
          "lai_path", errmsg)
        if (status /= NML_OK) return
        if (this%lai_path(idx(1)) == repeat(achar(0), len(this%lai_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%lai_path == repeat(achar(0), len(this%lai_path)))) status = NML_ERR_NOT_SET
      end if
    case ("soil_lut_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%soil_lut_path), ubound(this%soil_lut_path), &
          "soil_lut_path", errmsg)
        if (status /= NML_OK) return
        if (this%soil_lut_path(idx(1)) == repeat(achar(0), len(this%soil_lut_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%soil_lut_path == repeat(achar(0), len(this%soil_lut_path)))) status = NML_ERR_NOT_SET
      end if
    case ("geo_lut_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%geo_lut_path), ubound(this%geo_lut_path), &
          "geo_lut_path", errmsg)
        if (status /= NML_OK) return
        if (this%geo_lut_path(idx(1)) == repeat(achar(0), len(this%geo_lut_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%geo_lut_path == repeat(achar(0), len(this%geo_lut_path)))) status = NML_ERR_NOT_SET
      end if
    case ("lai_lut_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%lai_lut_path), ubound(this%lai_lut_path), &
          "lai_lut_path", errmsg)
        if (status /= NML_OK) return
        if (this%lai_lut_path(idx(1)) == repeat(achar(0), len(this%lai_lut_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%lai_lut_path == repeat(achar(0), len(this%lai_lut_path)))) status = NML_ERR_NOT_SET
      end if
    case default
      status = NML_ERR_INVALID_NAME
      if (present(errmsg)) errmsg = "unknown field: " // trim(name)
    end select
    if (status == NML_ERR_NOT_SET .and. present(errmsg)) then
      if (len_trim(errmsg) == 0) errmsg = "field not set: " // trim(name)
    end if
  end function nml_config_mpr_is_set

  !> \brief Determine the filled shape along flexible dimensions
  integer function nml_config_mpr_filled_shape(this, name, filled, errmsg) result(status)
    class(nml_config_mpr_t), intent(in) :: this
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
    case ("n_horizons")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'n_horizons'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%n_horizons, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%n_horizons, 1), &
        lbound(this%n_horizons, 1), -1
        if (.not. (this%n_horizons(idx) == -huge(this%n_horizons(idx)))) then
          filled(1) = idx - lbound(this%n_horizons, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%n_horizons, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%n_horizons(lb_1:ub_1) == -huge(this%n_horizons(lb_1:ub_1)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: n_horizons"
          return
        end if
      end if
    case ("tillage_depth")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'tillage_depth'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%tillage_depth, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%tillage_depth, 1), &
        lbound(this%tillage_depth, 1), -1
        if (.not. (this%tillage_depth(idx) == -huge(this%tillage_depth(idx)))) then
          filled(1) = idx - lbound(this%tillage_depth, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%tillage_depth, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%tillage_depth(lb_1:ub_1) == -huge(this%tillage_depth(lb_1:ub_1)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: tillage_depth"
          return
        end if
      end if
    case ("soil_depth")
      if (size(filled) /= 2) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'soil_depth'"
        return
      end if
      do dim = 1, 2
        filled(dim) = size(this%soil_depth, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%soil_depth, 1), &
        lbound(this%soil_depth, 1), -1
        if (.not. (all(this%soil_depth(idx, :) == -huge(this%soil_depth(idx, :))))) then
          filled(1) = idx - lbound(this%soil_depth, 1) + 1
          exit
        end if
      end do
      filled(2) = 0
      do idx = ubound(this%soil_depth, 2), &
        lbound(this%soil_depth, 2), -1
        if (.not. (all(this%soil_depth(:, idx) == -huge(this%soil_depth(:, idx))))) then
          filled(2) = idx - lbound(this%soil_depth, 2) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%soil_depth, 1)
        ub_1 = lb_1 + filled(1) - 1
        lb_2 = lbound(this%soil_depth, 2)
        ub_2 = lb_2 + filled(2) - 1
        if (any(this%soil_depth(lb_1:ub_1, lb_2:ub_2) == -huge(this%soil_depth(lb_1:ub_1, lb_2:ub_2)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: soil_depth"
          return
        end if
      end if
    case ("fracSealed_cityArea")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'fracSealed_cityArea'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%fracSealed_cityArea, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%fracSealed_cityArea, 1), &
        lbound(this%fracSealed_cityArea, 1), -1
        if (.not. (ieee_is_nan(this%fracSealed_cityArea(idx)))) then
          filled(1) = idx - lbound(this%fracSealed_cityArea, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%fracSealed_cityArea, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(ieee_is_nan(this%fracSealed_cityArea(lb_1:ub_1)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: fracSealed_cityArea"
          return
        end if
      end if
    case ("land_cover_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'land_cover_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%land_cover_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%land_cover_path, 1), &
        lbound(this%land_cover_path, 1), -1
        if (.not. (this%land_cover_path(idx) == repeat(achar(0), len(this%land_cover_path)))) then
          filled(1) = idx - lbound(this%land_cover_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%land_cover_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%land_cover_path(lb_1:ub_1) == repeat(achar(0), len(this%land_cover_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: land_cover_path"
          return
        end if
      end if
    case ("lai_time_step")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'lai_time_step'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%lai_time_step, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%lai_time_step, 1), &
        lbound(this%lai_time_step, 1), -1
        if (.not. (this%lai_time_step(idx) == -huge(this%lai_time_step(idx)))) then
          filled(1) = idx - lbound(this%lai_time_step, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%lai_time_step, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%lai_time_step(lb_1:ub_1) == -huge(this%lai_time_step(lb_1:ub_1)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: lai_time_step"
          return
        end if
      end if
    case ("lai_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'lai_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%lai_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%lai_path, 1), &
        lbound(this%lai_path, 1), -1
        if (.not. (this%lai_path(idx) == repeat(achar(0), len(this%lai_path)))) then
          filled(1) = idx - lbound(this%lai_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%lai_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%lai_path(lb_1:ub_1) == repeat(achar(0), len(this%lai_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: lai_path"
          return
        end if
      end if
    case ("soil_lut_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'soil_lut_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%soil_lut_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%soil_lut_path, 1), &
        lbound(this%soil_lut_path, 1), -1
        if (.not. (this%soil_lut_path(idx) == repeat(achar(0), len(this%soil_lut_path)))) then
          filled(1) = idx - lbound(this%soil_lut_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%soil_lut_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%soil_lut_path(lb_1:ub_1) == repeat(achar(0), len(this%soil_lut_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: soil_lut_path"
          return
        end if
      end if
    case ("geo_lut_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'geo_lut_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%geo_lut_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%geo_lut_path, 1), &
        lbound(this%geo_lut_path, 1), -1
        if (.not. (this%geo_lut_path(idx) == repeat(achar(0), len(this%geo_lut_path)))) then
          filled(1) = idx - lbound(this%geo_lut_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%geo_lut_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%geo_lut_path(lb_1:ub_1) == repeat(achar(0), len(this%geo_lut_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: geo_lut_path"
          return
        end if
      end if
    case ("lai_lut_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'lai_lut_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%lai_lut_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%lai_lut_path, 1), &
        lbound(this%lai_lut_path, 1), -1
        if (.not. (this%lai_lut_path(idx) == repeat(achar(0), len(this%lai_lut_path)))) then
          filled(1) = idx - lbound(this%lai_lut_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%lai_lut_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%lai_lut_path(lb_1:ub_1) == repeat(achar(0), len(this%lai_lut_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: lai_lut_path"
          return
        end if
      end if
    case default
      status = NML_ERR_INVALID_NAME
      if (present(errmsg)) errmsg = "field is not a flexible array: " // trim(name)
    end select
  end function nml_config_mpr_filled_shape

  !> \brief Validate required values and constraints
  integer function nml_config_mpr_is_valid(this, errmsg) result(status)
    class(nml_config_mpr_t), intent(in) :: this
    character(len=*), intent(out), optional :: errmsg
    integer :: istat
    integer, allocatable :: filled(:)

    status = NML_OK
    if (present(errmsg)) errmsg = ""

    ! flexible arrays
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("n_horizons", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: n_horizons"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("tillage_depth", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: tillage_depth"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(2))
    istat = this%filled_shape("soil_depth", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: soil_depth"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("fracSealed_cityArea", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: fracSealed_cityArea"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("land_cover_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: land_cover_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("lai_time_step", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: lai_time_step"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("lai_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: lai_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("soil_lut_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: soil_lut_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("geo_lut_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: geo_lut_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("lai_lut_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: lai_lut_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    ! enum constraints
    if (.not. all(soil_db_mode_in_enum(this%soil_db_mode, allow_missing=.true.))) then
      status = NML_ERR_ENUM
      if (present(errmsg)) errmsg = "enum constraint failed: soil_db_mode"
      return
    end if
    if (.not. all(lai_time_step_in_enum(this%lai_time_step, allow_missing=.true.))) then
      status = NML_ERR_ENUM
      if (present(errmsg)) errmsg = "enum constraint failed: lai_time_step"
      return
    end if
  end function nml_config_mpr_is_valid

end module nml_config_mpr
