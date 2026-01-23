!> \file nml_output_mhm.f90
!> \copydoc nml_output_mhm

!> \brief mHM output configuration
!> \details Output configuration for mHM.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Jan 2026
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_namelists
module nml_output_mhm
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
  integer(i4), parameter, public :: output_deflate_level_default = 6_i4
  logical, parameter, public :: output_double_precision_default = .false.
  integer(i4), parameter, public :: output_time_reference_default = 2_i4
  integer(i4), parameter, public :: output_frequency_default = -2_i4
  logical, parameter, public :: out_interception_default = .false.
  logical, parameter, public :: out_snowpack_default = .false.
  logical, parameter, public :: out_SWC_default = .false.
  logical, parameter, public :: out_SM_default = .false.
  logical, parameter, public :: out_SM_all_default = .false.
  logical, parameter, public :: out_sealedSTW_default = .false.
  logical, parameter, public :: out_unsatSTW_default = .false.
  logical, parameter, public :: out_satSTW_default = .false.
  logical, parameter, public :: out_PET_default = .false.
  logical, parameter, public :: out_aET_all_default = .false.
  logical, parameter, public :: out_Q_default = .false.
  logical, parameter, public :: out_QD_default = .false.
  logical, parameter, public :: out_QIf_default = .false.
  logical, parameter, public :: out_QIs_default = .false.
  logical, parameter, public :: out_QB_default = .false.
  logical, parameter, public :: out_recharge_default = .false.
  logical, parameter, public :: out_soil_infil_default = .false.
  logical, parameter, public :: out_neutrons_default = .false.
  logical, parameter, public :: out_aET_layer_default = .false.
  logical, parameter, public :: out_preEffect_default = .false.
  logical, parameter, public :: out_Qsm_default = .false.

  ! enum values
  integer(i4), parameter, public :: output_time_reference_enum_values(3) = [0_i4, 1_i4, 2_i4]

  ! bounds values
  integer(i4), parameter, public :: output_deflate_level_min = 0_i4
  integer(i4), parameter, public :: output_deflate_level_max = 9_i4
  integer(i4), parameter, public :: output_frequency_min = -3_i4

  !> \class nml_output_mhm_t
  !> \brief mHM output configuration
  !> \details Output configuration for mHM.
  type, public :: nml_output_mhm_t
    logical :: is_configured = .false. !< whether the namelist has been configured
    integer(i4) :: output_deflate_level !< Output deflate level
    logical :: output_double_precision !< Output double precision
    integer(i4) :: output_time_reference !< Output time reference
    integer(i4) :: output_frequency !< Output time step
    logical :: out_interception !< Interception
    logical :: out_snowpack !< Snowpack
    logical :: out_SWC !< Layered Soil Water Content
    logical :: out_SM !< Layered Volumetric Soil Moisture
    logical :: out_SM_all !< Mean Volumetric Soil Moisture
    logical :: out_sealedSTW !< Reservoir of Sealed areas
    logical :: out_unsatSTW !< Reservoir of Unsaturated areas
    logical :: out_satSTW !< Reservoir of Saturated areas
    logical :: out_PET !< Potential Evapotranspiration
    logical :: out_aET_all !< Mean actual Evapotranspiration
    logical :: out_Q !< Total Discharge
    logical :: out_QD !< Direct Runoff
    logical :: out_QIf !< Fast Interflow
    logical :: out_QIs !< Slow Interflow
    logical :: out_QB !< Baseflow
    logical :: out_recharge !< Groundwater Recharge
    logical :: out_soil_infil !< Soil Infiltration
    logical :: out_neutrons !< Neutrons
    logical :: out_aET_layer !< Actual Evapotranspiration from Soil Layers
    logical :: out_preEffect !< Effective Precipitation
    logical :: out_Qsm !< Snow Melt
  contains
    procedure :: init => nml_output_mhm_init
    procedure :: from_file => nml_output_mhm_from_file
    procedure :: set => nml_output_mhm_set
    procedure :: is_set => nml_output_mhm_is_set
    procedure :: is_valid => nml_output_mhm_is_valid
  end type nml_output_mhm_t

contains

  !> \brief Check whether a value is part of an enum
  elemental logical function output_time_reference_in_enum(val, allow_missing) result(in_enum)
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
    in_enum = any(val == output_time_reference_enum_values)
  end function output_time_reference_in_enum

  !> \brief Check whether a value is within bounds
  elemental logical function output_deflate_level_in_bounds(val, allow_missing) result(in_bounds)
    integer(i4), intent(in) :: val
    logical, intent(in), optional :: allow_missing

    if (present(allow_missing)) then
      if (allow_missing) then
        if (val == -huge(val)) then
          in_bounds = .true.
          return
        end if
      end if
    end if

    in_bounds = .true.
    if (val < output_deflate_level_min) in_bounds = .false.
    if (val > output_deflate_level_max) in_bounds = .false.
  end function output_deflate_level_in_bounds

  !> \brief Check whether a value is within bounds
  elemental logical function output_frequency_in_bounds(val, allow_missing) result(in_bounds)
    integer(i4), intent(in) :: val
    logical, intent(in), optional :: allow_missing

    if (present(allow_missing)) then
      if (allow_missing) then
        if (val == -huge(val)) then
          in_bounds = .true.
          return
        end if
      end if
    end if

    in_bounds = .true.
    if (val < output_frequency_min) in_bounds = .false.
  end function output_frequency_in_bounds

  !> \brief Initialize defaults and sentinels for output_mhm
  integer function nml_output_mhm_init(this, errmsg) result(status)
    class(nml_output_mhm_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    this%is_configured = .false.

    ! default values
    this%output_deflate_level = output_deflate_level_default
    this%output_double_precision = output_double_precision_default ! bool values always need a default
    this%output_time_reference = output_time_reference_default
    this%output_frequency = output_frequency_default
    this%out_interception = out_interception_default ! bool values always need a default
    this%out_snowpack = out_snowpack_default ! bool values always need a default
    this%out_SWC = out_SWC_default ! bool values always need a default
    this%out_SM = out_SM_default ! bool values always need a default
    this%out_SM_all = out_SM_all_default ! bool values always need a default
    this%out_sealedSTW = out_sealedSTW_default ! bool values always need a default
    this%out_unsatSTW = out_unsatSTW_default ! bool values always need a default
    this%out_satSTW = out_satSTW_default ! bool values always need a default
    this%out_PET = out_PET_default ! bool values always need a default
    this%out_aET_all = out_aET_all_default ! bool values always need a default
    this%out_Q = out_Q_default ! bool values always need a default
    this%out_QD = out_QD_default ! bool values always need a default
    this%out_QIf = out_QIf_default ! bool values always need a default
    this%out_QIs = out_QIs_default ! bool values always need a default
    this%out_QB = out_QB_default ! bool values always need a default
    this%out_recharge = out_recharge_default ! bool values always need a default
    this%out_soil_infil = out_soil_infil_default ! bool values always need a default
    this%out_neutrons = out_neutrons_default ! bool values always need a default
    this%out_aET_layer = out_aET_layer_default ! bool values always need a default
    this%out_preEffect = out_preEffect_default ! bool values always need a default
    this%out_Qsm = out_Qsm_default ! bool values always need a default
  end function nml_output_mhm_init

  !> \brief Read output_mhm namelist from file
  integer function nml_output_mhm_from_file(this, file, errmsg) result(status)
    class(nml_output_mhm_t), intent(inout) :: this
    character(len=*), intent(in) :: file !< path to namelist file
    character(len=*), intent(out), optional :: errmsg
    ! namelist variables
    integer(i4) :: output_deflate_level
    logical :: output_double_precision
    integer(i4) :: output_time_reference
    integer(i4) :: output_frequency
    logical :: out_interception
    logical :: out_snowpack
    logical :: out_SWC
    logical :: out_SM
    logical :: out_SM_all
    logical :: out_sealedSTW
    logical :: out_unsatSTW
    logical :: out_satSTW
    logical :: out_PET
    logical :: out_aET_all
    logical :: out_Q
    logical :: out_QD
    logical :: out_QIf
    logical :: out_QIs
    logical :: out_QB
    logical :: out_recharge
    logical :: out_soil_infil
    logical :: out_neutrons
    logical :: out_aET_layer
    logical :: out_preEffect
    logical :: out_Qsm
    ! locals
    type(nml_file_t) :: nml
    integer :: iostat
    integer :: close_status
    character(len=nml_line_buffer) :: iomsg

    namelist /output_mhm/ &
      output_deflate_level, &
      output_double_precision, &
      output_time_reference, &
      output_frequency, &
      out_interception, &
      out_snowpack, &
      out_SWC, &
      out_SM, &
      out_SM_all, &
      out_sealedSTW, &
      out_unsatSTW, &
      out_satSTW, &
      out_PET, &
      out_aET_all, &
      out_Q, &
      out_QD, &
      out_QIf, &
      out_QIs, &
      out_QB, &
      out_recharge, &
      out_soil_infil, &
      out_neutrons, &
      out_aET_layer, &
      out_preEffect, &
      out_Qsm

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return
    output_deflate_level = this%output_deflate_level
    output_double_precision = this%output_double_precision
    output_time_reference = this%output_time_reference
    output_frequency = this%output_frequency
    out_interception = this%out_interception
    out_snowpack = this%out_snowpack
    out_SWC = this%out_SWC
    out_SM = this%out_SM
    out_SM_all = this%out_SM_all
    out_sealedSTW = this%out_sealedSTW
    out_unsatSTW = this%out_unsatSTW
    out_satSTW = this%out_satSTW
    out_PET = this%out_PET
    out_aET_all = this%out_aET_all
    out_Q = this%out_Q
    out_QD = this%out_QD
    out_QIf = this%out_QIf
    out_QIs = this%out_QIs
    out_QB = this%out_QB
    out_recharge = this%out_recharge
    out_soil_infil = this%out_soil_infil
    out_neutrons = this%out_neutrons
    out_aET_layer = this%out_aET_layer
    out_preEffect = this%out_preEffect
    out_Qsm = this%out_Qsm

    status = nml%open(file, errmsg=errmsg)
    if (status /= NML_OK) return

    status = nml%find("output_mhm", errmsg=errmsg)
    if (status /= NML_OK) then
      close_status = nml%close()
      return
    end if

    ! read namelist
    read(nml%unit, nml=output_mhm, iostat=iostat, iomsg=iomsg)
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
    this%output_deflate_level = output_deflate_level
    this%output_double_precision = output_double_precision
    this%output_time_reference = output_time_reference
    this%output_frequency = output_frequency
    this%out_interception = out_interception
    this%out_snowpack = out_snowpack
    this%out_SWC = out_SWC
    this%out_SM = out_SM
    this%out_SM_all = out_SM_all
    this%out_sealedSTW = out_sealedSTW
    this%out_unsatSTW = out_unsatSTW
    this%out_satSTW = out_satSTW
    this%out_PET = out_PET
    this%out_aET_all = out_aET_all
    this%out_Q = out_Q
    this%out_QD = out_QD
    this%out_QIf = out_QIf
    this%out_QIs = out_QIs
    this%out_QB = out_QB
    this%out_recharge = out_recharge
    this%out_soil_infil = out_soil_infil
    this%out_neutrons = out_neutrons
    this%out_aET_layer = out_aET_layer
    this%out_preEffect = out_preEffect
    this%out_Qsm = out_Qsm

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_output_mhm_from_file

  !> \brief Set output_mhm values
  integer function nml_output_mhm_set(this, &
    output_deflate_level, &
    output_double_precision, &
    output_time_reference, &
    output_frequency, &
    out_interception, &
    out_snowpack, &
    out_SWC, &
    out_SM, &
    out_SM_all, &
    out_sealedSTW, &
    out_unsatSTW, &
    out_satSTW, &
    out_PET, &
    out_aET_all, &
    out_Q, &
    out_QD, &
    out_QIf, &
    out_QIs, &
    out_QB, &
    out_recharge, &
    out_soil_infil, &
    out_neutrons, &
    out_aET_layer, &
    out_preEffect, &
    out_Qsm, &
    errmsg) result(status)

    class(nml_output_mhm_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg
    integer(i4), intent(in), optional :: output_deflate_level
    logical, intent(in), optional :: output_double_precision
    integer(i4), intent(in), optional :: output_time_reference
    integer(i4), intent(in), optional :: output_frequency
    logical, intent(in), optional :: out_interception
    logical, intent(in), optional :: out_snowpack
    logical, intent(in), optional :: out_SWC
    logical, intent(in), optional :: out_SM
    logical, intent(in), optional :: out_SM_all
    logical, intent(in), optional :: out_sealedSTW
    logical, intent(in), optional :: out_unsatSTW
    logical, intent(in), optional :: out_satSTW
    logical, intent(in), optional :: out_PET
    logical, intent(in), optional :: out_aET_all
    logical, intent(in), optional :: out_Q
    logical, intent(in), optional :: out_QD
    logical, intent(in), optional :: out_QIf
    logical, intent(in), optional :: out_QIs
    logical, intent(in), optional :: out_QB
    logical, intent(in), optional :: out_recharge
    logical, intent(in), optional :: out_soil_infil
    logical, intent(in), optional :: out_neutrons
    logical, intent(in), optional :: out_aET_layer
    logical, intent(in), optional :: out_preEffect
    logical, intent(in), optional :: out_Qsm

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return

    ! required parameters
    ! override with provided values
    if (present(output_deflate_level)) this%output_deflate_level = output_deflate_level
    if (present(output_double_precision)) this%output_double_precision = output_double_precision
    if (present(output_time_reference)) this%output_time_reference = output_time_reference
    if (present(output_frequency)) this%output_frequency = output_frequency
    if (present(out_interception)) this%out_interception = out_interception
    if (present(out_snowpack)) this%out_snowpack = out_snowpack
    if (present(out_SWC)) this%out_SWC = out_SWC
    if (present(out_SM)) this%out_SM = out_SM
    if (present(out_SM_all)) this%out_SM_all = out_SM_all
    if (present(out_sealedSTW)) this%out_sealedSTW = out_sealedSTW
    if (present(out_unsatSTW)) this%out_unsatSTW = out_unsatSTW
    if (present(out_satSTW)) this%out_satSTW = out_satSTW
    if (present(out_PET)) this%out_PET = out_PET
    if (present(out_aET_all)) this%out_aET_all = out_aET_all
    if (present(out_Q)) this%out_Q = out_Q
    if (present(out_QD)) this%out_QD = out_QD
    if (present(out_QIf)) this%out_QIf = out_QIf
    if (present(out_QIs)) this%out_QIs = out_QIs
    if (present(out_QB)) this%out_QB = out_QB
    if (present(out_recharge)) this%out_recharge = out_recharge
    if (present(out_soil_infil)) this%out_soil_infil = out_soil_infil
    if (present(out_neutrons)) this%out_neutrons = out_neutrons
    if (present(out_aET_layer)) this%out_aET_layer = out_aET_layer
    if (present(out_preEffect)) this%out_preEffect = out_preEffect
    if (present(out_Qsm)) this%out_Qsm = out_Qsm

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_output_mhm_set

  !> \brief Check whether a namelist value was set
  integer function nml_output_mhm_is_set(this, name, idx, errmsg) result(status)
    class(nml_output_mhm_t), intent(in) :: this
    character(len=*), intent(in) :: name
    integer, intent(in), optional :: idx(:)
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    select case (trim(name))
    case ("output_deflate_level")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'output_deflate_level'"
        return
      end if
    case ("output_double_precision")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'output_double_precision'"
        return
      end if
    case ("output_time_reference")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'output_time_reference'"
        return
      end if
    case ("output_frequency")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'output_frequency'"
        return
      end if
    case ("out_interception")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'out_interception'"
        return
      end if
    case ("out_snowpack")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'out_snowpack'"
        return
      end if
    case ("out_SWC")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'out_SWC'"
        return
      end if
    case ("out_SM")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'out_SM'"
        return
      end if
    case ("out_SM_all")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'out_SM_all'"
        return
      end if
    case ("out_sealedSTW")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'out_sealedSTW'"
        return
      end if
    case ("out_unsatSTW")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'out_unsatSTW'"
        return
      end if
    case ("out_satSTW")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'out_satSTW'"
        return
      end if
    case ("out_PET")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'out_PET'"
        return
      end if
    case ("out_aET_all")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'out_aET_all'"
        return
      end if
    case ("out_Q")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'out_Q'"
        return
      end if
    case ("out_QD")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'out_QD'"
        return
      end if
    case ("out_QIf")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'out_QIf'"
        return
      end if
    case ("out_QIs")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'out_QIs'"
        return
      end if
    case ("out_QB")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'out_QB'"
        return
      end if
    case ("out_recharge")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'out_recharge'"
        return
      end if
    case ("out_soil_infil")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'out_soil_infil'"
        return
      end if
    case ("out_neutrons")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'out_neutrons'"
        return
      end if
    case ("out_aET_layer")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'out_aET_layer'"
        return
      end if
    case ("out_preEffect")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'out_preEffect'"
        return
      end if
    case ("out_Qsm")
      if (present(idx)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "index not supported for 'out_Qsm'"
        return
      end if
    case default
      status = NML_ERR_INVALID_NAME
      if (present(errmsg)) errmsg = "unknown field: " // trim(name)
    end select
    if (status == NML_ERR_NOT_SET .and. present(errmsg)) then
      if (len_trim(errmsg) == 0) errmsg = "field not set: " // trim(name)
    end if
  end function nml_output_mhm_is_set

  !> \brief Validate required values and constraints
  integer function nml_output_mhm_is_valid(this, errmsg) result(status)
    class(nml_output_mhm_t), intent(in) :: this
    character(len=*), intent(out), optional :: errmsg
    integer :: istat

    status = NML_OK
    if (present(errmsg)) errmsg = ""

    ! enum constraints
    istat = this%is_set("output_time_reference", errmsg=errmsg)
    if (istat == NML_OK) then
      if (.not. output_time_reference_in_enum(this%output_time_reference)) then
        status = NML_ERR_ENUM
        if (present(errmsg)) errmsg = "enum constraint failed: output_time_reference"
        return
      end if
    else if (istat /= NML_ERR_NOT_SET) then
      status = istat
      return
    end if
    ! bounds constraints
    istat = this%is_set("output_deflate_level", errmsg=errmsg)
    if (istat == NML_OK) then
      if (.not. output_deflate_level_in_bounds(this%output_deflate_level)) then
        status = NML_ERR_BOUNDS
        if (present(errmsg)) errmsg = "bounds constraint failed: output_deflate_level"
        return
      end if
    else if (istat /= NML_ERR_NOT_SET) then
      status = istat
      return
    end if
    istat = this%is_set("output_frequency", errmsg=errmsg)
    if (istat == NML_OK) then
      if (.not. output_frequency_in_bounds(this%output_frequency)) then
        status = NML_ERR_BOUNDS
        if (present(errmsg)) errmsg = "bounds constraint failed: output_frequency"
        return
      end if
    else if (istat /= NML_ERR_NOT_SET) then
      status = istat
      return
    end if
  end function nml_output_mhm_is_valid

end module nml_output_mhm
