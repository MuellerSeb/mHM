!> \file nml_parameter_geoparameter.f90
!> \copydoc nml_geoparameter

!> \brief Geological parameters
!> \details Parameters for geoparameter.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Jan 2026
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_namelists
module nml_geoparameter
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
    max_geo_units, &
    NML_ERR_PARTLY_SET
  use ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_is_nan
  ! kind specifiers listed in the nml-tools configuration file
  use mo_kind, only: &
    dp

  implicit none

  !> \class nml_geoparameter_t
  !> \brief Geological parameters
  !> \details Parameters for geoparameter.
  type, public :: nml_geoparameter_t
    logical :: is_configured = .false. !< whether the namelist has been configured
    real(dp), dimension(5, max_geo_units) :: GeoParam !< Geological parameters
  contains
    procedure :: init => nml_geoparameter_init
    procedure :: from_file => nml_geoparameter_from_file
    procedure :: set => nml_geoparameter_set
    procedure :: is_set => nml_geoparameter_is_set
    procedure :: filled_shape => nml_geoparameter_filled_shape
    procedure :: is_valid => nml_geoparameter_is_valid
  end type nml_geoparameter_t

contains

  !> \brief Initialize defaults and sentinels for geoparameter
  integer function nml_geoparameter_init(this, errmsg) result(status)
    class(nml_geoparameter_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    this%is_configured = .false.

    ! sentinel values for required/optional parameters
    this%GeoParam = ieee_value(this%GeoParam, ieee_quiet_nan) ! sentinel for required real array
  end function nml_geoparameter_init

  !> \brief Read geoparameter namelist from file
  integer function nml_geoparameter_from_file(this, file, errmsg) result(status)
    class(nml_geoparameter_t), intent(inout) :: this
    character(len=*), intent(in) :: file !< path to namelist file
    character(len=*), intent(out), optional :: errmsg
    ! namelist variables
    real(dp), dimension(5, max_geo_units) :: GeoParam
    ! locals
    type(nml_file_t) :: nml
    integer :: iostat
    integer :: close_status
    character(len=nml_line_buffer) :: iomsg

    namelist /geoparameter/ &
      GeoParam

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return
    GeoParam = this%GeoParam

    status = nml%open(file, errmsg=errmsg)
    if (status /= NML_OK) return

    status = nml%find("geoparameter", errmsg=errmsg)
    if (status /= NML_OK) then
      close_status = nml%close()
      return
    end if

    ! read namelist
    read(nml%unit, nml=geoparameter, iostat=iostat, iomsg=iomsg)
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
    this%GeoParam = GeoParam

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_geoparameter_from_file

  !> \brief Set geoparameter values
  integer function nml_geoparameter_set(this, &
    GeoParam, &
    errmsg) result(status)

    class(nml_geoparameter_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg
    real(dp), dimension(:, :), intent(in) :: GeoParam
    integer :: &
      lb_2, &
      ub_2

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return

    ! required parameters
    if (size(GeoParam, 1) /= size(this%GeoParam, 1)) then
      status = NML_ERR_INVALID_INDEX
      if (present(errmsg)) errmsg = "dimension 1 mismatch for 'GeoParam'"
      return
    end if
    if (size(GeoParam, 2) > size(this%GeoParam, 2)) then
      status = NML_ERR_INVALID_INDEX
      if (present(errmsg)) errmsg = "dimension 2 exceeds bounds for 'GeoParam'"
      return
    end if
    lb_2 = lbound(this%GeoParam, 2)
    ub_2 = lb_2 + size(GeoParam, 2) - 1
    this%GeoParam(:, lb_2:ub_2) = GeoParam

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_geoparameter_set

  !> \brief Check whether a namelist value was set
  integer function nml_geoparameter_is_set(this, name, idx, errmsg) result(status)
    class(nml_geoparameter_t), intent(in) :: this
    character(len=*), intent(in) :: name
    integer, intent(in), optional :: idx(:)
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    select case (trim(name))
    case ("GeoParam")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%GeoParam), ubound(this%GeoParam), &
          "GeoParam", errmsg)
        if (status /= NML_OK) return
        if (ieee_is_nan(this%GeoParam(idx(1), idx(2)))) status = NML_ERR_NOT_SET
      else
        if (all(ieee_is_nan(this%GeoParam))) status = NML_ERR_NOT_SET
      end if
    case default
      status = NML_ERR_INVALID_NAME
      if (present(errmsg)) errmsg = "unknown field: " // trim(name)
    end select
    if (status == NML_ERR_NOT_SET .and. present(errmsg)) then
      if (len_trim(errmsg) == 0) errmsg = "field not set: " // trim(name)
    end if
  end function nml_geoparameter_is_set

  !> \brief Determine the filled shape along flexible dimensions
  integer function nml_geoparameter_filled_shape(this, name, filled, errmsg) result(status)
    class(nml_geoparameter_t), intent(in) :: this
    character(len=*), intent(in) :: name
    integer, intent(out) :: filled(:)
    character(len=*), intent(out), optional :: errmsg
    integer :: idx
    integer :: dim
    integer :: &
      lb_2, &
      ub_2

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    select case (trim(name))
    case ("GeoParam")
      if (size(filled) /= 2) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'GeoParam'"
        return
      end if
      do dim = 1, 2
        filled(dim) = size(this%GeoParam, dim)
      end do
      filled(2) = 0
      do idx = ubound(this%GeoParam, 2), &
        lbound(this%GeoParam, 2), -1
        if (.not. (all(ieee_is_nan(this%GeoParam(:, idx))))) then
          filled(2) = idx - lbound(this%GeoParam, 2) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_2 = lbound(this%GeoParam, 2)
        ub_2 = lb_2 + filled(2) - 1
        if (any(ieee_is_nan(this%GeoParam(:, lb_2:ub_2)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: GeoParam"
          return
        end if
      end if
    case default
      status = NML_ERR_INVALID_NAME
      if (present(errmsg)) errmsg = "field is not a flexible array: " // trim(name)
    end select
  end function nml_geoparameter_filled_shape

  !> \brief Validate required values and constraints
  integer function nml_geoparameter_is_valid(this, errmsg) result(status)
    class(nml_geoparameter_t), intent(in) :: this
    character(len=*), intent(out), optional :: errmsg
    integer :: istat
    integer, allocatable :: filled(:)

    status = NML_OK
    if (present(errmsg)) errmsg = ""

    ! flexible arrays
    if (allocated(filled)) deallocate(filled)
    allocate(filled(2))
    istat = this%filled_shape("GeoParam", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: GeoParam"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (minval(filled) == 0) then
      status = NML_ERR_REQUIRED
      if (present(errmsg)) errmsg = "required field not set: GeoParam"
      return
    end if
  end function nml_geoparameter_is_valid

end module nml_geoparameter
