!> \file nml_config_input.f90
!> \copydoc nml_config_input

!> \brief Input configuration
!> \details Paths and variable names for input data used by mHM.
!! Arrays are indexed by domain (dimension 1). Most paths are optional.
!! Variable name entries define the NetCDF variable names to read.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Jan 2026
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_namelists
module nml_config_input
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
    i4

  implicit none

  ! default values
  integer(i4), parameter, public :: chunking_default = 0_i4
  integer(i4), parameter, public :: time_stamp_location_default = 0_i4
  logical, parameter, public :: morph_latlon_default = .false.
  character(len=buf), parameter, public :: pre_var_default = "pre"
  character(len=buf), parameter, public :: pet_var_default = "pet"
  character(len=buf), parameter, public :: temp_var_default = "tavg"
  character(len=buf), parameter, public :: tann_var_default = "tann"
  character(len=buf), parameter, public :: tmin_var_default = "tmin"
  character(len=buf), parameter, public :: tmax_var_default = "tmax"
  character(len=buf), parameter, public :: ssrd_var_default = "ssrd"
  character(len=buf), parameter, public :: strd_var_default = "strd"
  character(len=buf), parameter, public :: netrad_var_default = "net_rad"
  character(len=buf), parameter, public :: eabs_var_default = "eabs"
  character(len=buf), parameter, public :: wind_var_default = "windspeed"
  character(len=buf), parameter, public :: meteo_mask_var_default = "mask"
  character(len=buf), parameter, public :: runoff_var_default = "runoff"
  character(len=buf), parameter, public :: runoff_sealed_var_default = "runoff_sealed"
  character(len=buf), parameter, public :: interflow_fast_var_default = "interflow_fast"
  character(len=buf), parameter, public :: interflow_slow_var_default = "interflow_slow"
  character(len=buf), parameter, public :: baseflow_var_default = "baseflow"
  character(len=buf), parameter, public :: hydro_mask_var_default = "mask"
  character(len=buf), parameter, public :: dem_var_default = "dem"
  character(len=buf), parameter, public :: slope_var_default = "slope"
  character(len=buf), parameter, public :: aspect_var_default = "aspect"
  character(len=buf), parameter, public :: geo_class_var_default = "geology_class"
  character(len=buf), parameter, public :: soil_class_var_default = "soil_class"
  character(len=buf), parameter, public :: lai_class_var_default = "LAI_class"
  character(len=buf), parameter, public :: river_width_var_default = "P_bkfl"
  character(len=buf), parameter, public :: morph_mask_var_default = "mask"
  character(len=buf), parameter, public :: hydro_lat_var_default = "lat"
  character(len=buf), parameter, public :: hydro_lon_var_default = "lon"
  character(len=buf), parameter, public :: morph_lat_var_default = "lat_l0"
  character(len=buf), parameter, public :: morph_lon_var_default = "lon_l0"
  character(len=buf), parameter, public :: route_lat_var_default = "lat_l11"
  character(len=buf), parameter, public :: route_lon_var_default = "lon_l11"

  ! enum values
  integer(i4), parameter, public :: time_stamp_location_enum_values(3) = [0_i4, 1_i4, 2_i4]

  ! bounds values
  integer(i4), parameter, public :: chunking_min = -3_i4

  !> \class nml_config_input_t
  !> \brief Input configuration
  !> \details Paths and variable names for input data used by mHM.
  !! Arrays are indexed by domain (dimension 1). Most paths are optional.
  !! Variable name entries define the NetCDF variable names to read.
  type, public :: nml_config_input_t
    logical :: is_configured = .false. !< whether the namelist has been configured
    integer(i4), dimension(max_domains) :: chunking !< Chunking for input data
    integer(i4), dimension(max_domains) :: time_stamp_location !< NetCDF time-stamp location
    character(len=buf), dimension(max_domains) :: latlon_path !< Latlon specification file path
    logical, dimension(max_domains) :: morph_latlon !< DEM in latlon coordinates
    character(len=buf), dimension(max_domains) :: pre_path !< Precipitation input
    character(len=buf), dimension(max_domains) :: pet_path !< Potential evapotranspiration input
    character(len=buf), dimension(max_domains) :: temp_path !< Air temperature input
    character(len=buf), dimension(max_domains) :: tann_path !< Air temperature annual mean input
    character(len=buf), dimension(max_domains) :: tmin_path !< Air temperature daily minimum input
    character(len=buf), dimension(max_domains) :: tmax_path !< Air temperature daily maximum input
    character(len=buf), dimension(max_domains) :: ssrd_path !< Surface shortwave radiation downwards input
    character(len=buf), dimension(max_domains) :: strd_path !< Surface thermal radiation downwards input
    character(len=buf), dimension(max_domains) :: netrad_path !< Net radiation input
    character(len=buf), dimension(max_domains) :: eabs_path !< Vapor pressure input
    character(len=buf), dimension(max_domains) :: wind_path !< Wind speed input
    character(len=buf), dimension(max_domains) :: meteo_mask_path !< Meteorological mask file path
    character(len=buf), dimension(max_domains) :: runoff_path !< Runoff input
    character(len=buf), dimension(max_domains) :: runoff_sealed_path !< Sealed runoff input
    character(len=buf), dimension(max_domains) :: interflow_fast_path !< Fast interflow input
    character(len=buf), dimension(max_domains) :: interflow_slow_path !< Slow interflow input
    character(len=buf), dimension(max_domains) :: baseflow_path !< Baseflow input
    character(len=buf), dimension(max_domains) :: hydro_mask_path !< Hydrological mask file path
    character(len=buf), dimension(max_domains) :: dem_path !< DEM input
    character(len=buf), dimension(max_domains) :: slope_path !< Slope input
    character(len=buf), dimension(max_domains) :: aspect_path !< Aspect input
    character(len=buf), dimension(max_domains) :: geo_class_path !< Geology class input
    character(len=buf), dimension(max_domains) :: soil_class_path !< Soil class input
    character(len=buf), dimension(max_domains) :: soil_horizon_class_path !< Soil horizon class input
    character(len=buf), dimension(max_domains) :: lai_class_path !< LAI class input
    character(len=buf), dimension(max_domains) :: river_width_path !< River width input
    character(len=buf), dimension(max_domains) :: morph_mask_path !< Morphology mask file path
    character(len=buf), dimension(max_domains) :: pre_var !< Precipitation variable name
    character(len=buf), dimension(max_domains) :: pet_var !< Potential evapotranspiration variable name
    character(len=buf), dimension(max_domains) :: temp_var !< Air temperature variable name
    character(len=buf), dimension(max_domains) :: tann_var !< Air temperature annual mean variable name
    character(len=buf), dimension(max_domains) :: tmin_var !< Air temperature daily minimum variable name
    character(len=buf), dimension(max_domains) :: tmax_var !< Air temperature daily maximum variable name
    character(len=buf), dimension(max_domains) :: ssrd_var !< Surface shortwave radiation variable name
    character(len=buf), dimension(max_domains) :: strd_var !< Surface thermal radiation variable name
    character(len=buf), dimension(max_domains) :: netrad_var !< Net radiation variable name
    character(len=buf), dimension(max_domains) :: eabs_var !< Vapor pressure variable name
    character(len=buf), dimension(max_domains) :: wind_var !< Wind speed variable name
    character(len=buf), dimension(max_domains) :: meteo_mask_var !< Meteorological mask variable name
    character(len=buf), dimension(max_domains) :: runoff_var !< Runoff variable name
    character(len=buf), dimension(max_domains) :: runoff_sealed_var !< Sealed runoff variable name
    character(len=buf), dimension(max_domains) :: interflow_fast_var !< Fast interflow variable name
    character(len=buf), dimension(max_domains) :: interflow_slow_var !< Slow interflow variable name
    character(len=buf), dimension(max_domains) :: baseflow_var !< Baseflow variable name
    character(len=buf), dimension(max_domains) :: hydro_mask_var !< Hydrological mask variable name
    character(len=buf), dimension(max_domains) :: dem_var !< DEM variable name
    character(len=buf), dimension(max_domains) :: slope_var !< Slope variable name
    character(len=buf), dimension(max_domains) :: aspect_var !< Aspect variable name
    character(len=buf), dimension(max_domains) :: geo_class_var !< Geology class variable name
    character(len=buf), dimension(max_domains) :: soil_class_var !< Soil class variable name
    character(len=buf), dimension(max_domains) :: lai_class_var !< LAI class variable name
    character(len=buf), dimension(max_domains) :: river_width_var !< River width variable name
    character(len=buf), dimension(max_domains) :: morph_mask_var !< Morphology mask variable name
    character(len=buf), dimension(max_domains) :: hydro_lat_var !< Hydrological latitude variable name
    character(len=buf), dimension(max_domains) :: hydro_lon_var !< Hydrological longitude variable name
    character(len=buf), dimension(max_domains) :: morph_lat_var !< Morphology latitude variable name
    character(len=buf), dimension(max_domains) :: morph_lon_var !< Morphology longitude variable name
    character(len=buf), dimension(max_domains) :: route_lat_var !< Routing latitude variable name
    character(len=buf), dimension(max_domains) :: route_lon_var !< Routing longitude variable name
  contains
    procedure :: init => nml_config_input_init
    procedure :: from_file => nml_config_input_from_file
    procedure :: set => nml_config_input_set
    procedure :: is_set => nml_config_input_is_set
    procedure :: filled_shape => nml_config_input_filled_shape
    procedure :: is_valid => nml_config_input_is_valid
  end type nml_config_input_t

contains

  !> \brief Check whether a value is part of an enum
  elemental logical function time_stamp_location_in_enum(val, allow_missing) result(in_enum)
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
    in_enum = any(val == time_stamp_location_enum_values)
  end function time_stamp_location_in_enum

  !> \brief Check whether a value is within bounds
  elemental logical function chunking_in_bounds(val, allow_missing) result(in_bounds)
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
    if (val < chunking_min) in_bounds = .false.
  end function chunking_in_bounds

  !> \brief Initialize defaults and sentinels for config_input
  integer function nml_config_input_init(this, errmsg) result(status)
    class(nml_config_input_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    this%is_configured = .false.

    ! sentinel values for required/optional parameters
    this%latlon_path = repeat(achar(0), len(this%latlon_path)) ! sentinel for optional string array
    this%pre_path = repeat(achar(0), len(this%pre_path)) ! sentinel for optional string array
    this%pet_path = repeat(achar(0), len(this%pet_path)) ! sentinel for optional string array
    this%temp_path = repeat(achar(0), len(this%temp_path)) ! sentinel for optional string array
    this%tann_path = repeat(achar(0), len(this%tann_path)) ! sentinel for optional string array
    this%tmin_path = repeat(achar(0), len(this%tmin_path)) ! sentinel for optional string array
    this%tmax_path = repeat(achar(0), len(this%tmax_path)) ! sentinel for optional string array
    this%ssrd_path = repeat(achar(0), len(this%ssrd_path)) ! sentinel for optional string array
    this%strd_path = repeat(achar(0), len(this%strd_path)) ! sentinel for optional string array
    this%netrad_path = repeat(achar(0), len(this%netrad_path)) ! sentinel for optional string array
    this%eabs_path = repeat(achar(0), len(this%eabs_path)) ! sentinel for optional string array
    this%wind_path = repeat(achar(0), len(this%wind_path)) ! sentinel for optional string array
    this%meteo_mask_path = repeat(achar(0), len(this%meteo_mask_path)) ! sentinel for optional string array
    this%runoff_path = repeat(achar(0), len(this%runoff_path)) ! sentinel for optional string array
    this%runoff_sealed_path = repeat(achar(0), len(this%runoff_sealed_path)) ! sentinel for optional string array
    this%interflow_fast_path = repeat(achar(0), len(this%interflow_fast_path)) ! sentinel for optional string array
    this%interflow_slow_path = repeat(achar(0), len(this%interflow_slow_path)) ! sentinel for optional string array
    this%baseflow_path = repeat(achar(0), len(this%baseflow_path)) ! sentinel for optional string array
    this%hydro_mask_path = repeat(achar(0), len(this%hydro_mask_path)) ! sentinel for optional string array
    this%dem_path = repeat(achar(0), len(this%dem_path)) ! sentinel for optional string array
    this%slope_path = repeat(achar(0), len(this%slope_path)) ! sentinel for optional string array
    this%aspect_path = repeat(achar(0), len(this%aspect_path)) ! sentinel for optional string array
    this%geo_class_path = repeat(achar(0), len(this%geo_class_path)) ! sentinel for optional string array
    this%soil_class_path = repeat(achar(0), len(this%soil_class_path)) ! sentinel for optional string array
    this%soil_horizon_class_path = repeat(achar(0), len(this%soil_horizon_class_path)) ! sentinel for optional string array
    this%lai_class_path = repeat(achar(0), len(this%lai_class_path)) ! sentinel for optional string array
    this%river_width_path = repeat(achar(0), len(this%river_width_path)) ! sentinel for optional string array
    this%morph_mask_path = repeat(achar(0), len(this%morph_mask_path)) ! sentinel for optional string array
    ! default values
    this%chunking = chunking_default
    this%time_stamp_location = time_stamp_location_default
    this%morph_latlon = morph_latlon_default
    this%pre_var = pre_var_default
    this%pet_var = pet_var_default
    this%temp_var = temp_var_default
    this%tann_var = tann_var_default
    this%tmin_var = tmin_var_default
    this%tmax_var = tmax_var_default
    this%ssrd_var = ssrd_var_default
    this%strd_var = strd_var_default
    this%netrad_var = netrad_var_default
    this%eabs_var = eabs_var_default
    this%wind_var = wind_var_default
    this%meteo_mask_var = meteo_mask_var_default
    this%runoff_var = runoff_var_default
    this%runoff_sealed_var = runoff_sealed_var_default
    this%interflow_fast_var = interflow_fast_var_default
    this%interflow_slow_var = interflow_slow_var_default
    this%baseflow_var = baseflow_var_default
    this%hydro_mask_var = hydro_mask_var_default
    this%dem_var = dem_var_default
    this%slope_var = slope_var_default
    this%aspect_var = aspect_var_default
    this%geo_class_var = geo_class_var_default
    this%soil_class_var = soil_class_var_default
    this%lai_class_var = lai_class_var_default
    this%river_width_var = river_width_var_default
    this%morph_mask_var = morph_mask_var_default
    this%hydro_lat_var = hydro_lat_var_default
    this%hydro_lon_var = hydro_lon_var_default
    this%morph_lat_var = morph_lat_var_default
    this%morph_lon_var = morph_lon_var_default
    this%route_lat_var = route_lat_var_default
    this%route_lon_var = route_lon_var_default
  end function nml_config_input_init

  !> \brief Read config_input namelist from file
  integer function nml_config_input_from_file(this, file, errmsg) result(status)
    class(nml_config_input_t), intent(inout) :: this
    character(len=*), intent(in) :: file !< path to namelist file
    character(len=*), intent(out), optional :: errmsg
    ! namelist variables
    integer(i4), dimension(max_domains) :: chunking
    integer(i4), dimension(max_domains) :: time_stamp_location
    character(len=buf), dimension(max_domains) :: latlon_path
    logical, dimension(max_domains) :: morph_latlon
    character(len=buf), dimension(max_domains) :: pre_path
    character(len=buf), dimension(max_domains) :: pet_path
    character(len=buf), dimension(max_domains) :: temp_path
    character(len=buf), dimension(max_domains) :: tann_path
    character(len=buf), dimension(max_domains) :: tmin_path
    character(len=buf), dimension(max_domains) :: tmax_path
    character(len=buf), dimension(max_domains) :: ssrd_path
    character(len=buf), dimension(max_domains) :: strd_path
    character(len=buf), dimension(max_domains) :: netrad_path
    character(len=buf), dimension(max_domains) :: eabs_path
    character(len=buf), dimension(max_domains) :: wind_path
    character(len=buf), dimension(max_domains) :: meteo_mask_path
    character(len=buf), dimension(max_domains) :: runoff_path
    character(len=buf), dimension(max_domains) :: runoff_sealed_path
    character(len=buf), dimension(max_domains) :: interflow_fast_path
    character(len=buf), dimension(max_domains) :: interflow_slow_path
    character(len=buf), dimension(max_domains) :: baseflow_path
    character(len=buf), dimension(max_domains) :: hydro_mask_path
    character(len=buf), dimension(max_domains) :: dem_path
    character(len=buf), dimension(max_domains) :: slope_path
    character(len=buf), dimension(max_domains) :: aspect_path
    character(len=buf), dimension(max_domains) :: geo_class_path
    character(len=buf), dimension(max_domains) :: soil_class_path
    character(len=buf), dimension(max_domains) :: soil_horizon_class_path
    character(len=buf), dimension(max_domains) :: lai_class_path
    character(len=buf), dimension(max_domains) :: river_width_path
    character(len=buf), dimension(max_domains) :: morph_mask_path
    character(len=buf), dimension(max_domains) :: pre_var
    character(len=buf), dimension(max_domains) :: pet_var
    character(len=buf), dimension(max_domains) :: temp_var
    character(len=buf), dimension(max_domains) :: tann_var
    character(len=buf), dimension(max_domains) :: tmin_var
    character(len=buf), dimension(max_domains) :: tmax_var
    character(len=buf), dimension(max_domains) :: ssrd_var
    character(len=buf), dimension(max_domains) :: strd_var
    character(len=buf), dimension(max_domains) :: netrad_var
    character(len=buf), dimension(max_domains) :: eabs_var
    character(len=buf), dimension(max_domains) :: wind_var
    character(len=buf), dimension(max_domains) :: meteo_mask_var
    character(len=buf), dimension(max_domains) :: runoff_var
    character(len=buf), dimension(max_domains) :: runoff_sealed_var
    character(len=buf), dimension(max_domains) :: interflow_fast_var
    character(len=buf), dimension(max_domains) :: interflow_slow_var
    character(len=buf), dimension(max_domains) :: baseflow_var
    character(len=buf), dimension(max_domains) :: hydro_mask_var
    character(len=buf), dimension(max_domains) :: dem_var
    character(len=buf), dimension(max_domains) :: slope_var
    character(len=buf), dimension(max_domains) :: aspect_var
    character(len=buf), dimension(max_domains) :: geo_class_var
    character(len=buf), dimension(max_domains) :: soil_class_var
    character(len=buf), dimension(max_domains) :: lai_class_var
    character(len=buf), dimension(max_domains) :: river_width_var
    character(len=buf), dimension(max_domains) :: morph_mask_var
    character(len=buf), dimension(max_domains) :: hydro_lat_var
    character(len=buf), dimension(max_domains) :: hydro_lon_var
    character(len=buf), dimension(max_domains) :: morph_lat_var
    character(len=buf), dimension(max_domains) :: morph_lon_var
    character(len=buf), dimension(max_domains) :: route_lat_var
    character(len=buf), dimension(max_domains) :: route_lon_var
    ! locals
    type(nml_file_t) :: nml
    integer :: iostat
    integer :: close_status
    character(len=nml_line_buffer) :: iomsg

    namelist /config_input/ &
      chunking, &
      time_stamp_location, &
      latlon_path, &
      morph_latlon, &
      pre_path, &
      pet_path, &
      temp_path, &
      tann_path, &
      tmin_path, &
      tmax_path, &
      ssrd_path, &
      strd_path, &
      netrad_path, &
      eabs_path, &
      wind_path, &
      meteo_mask_path, &
      runoff_path, &
      runoff_sealed_path, &
      interflow_fast_path, &
      interflow_slow_path, &
      baseflow_path, &
      hydro_mask_path, &
      dem_path, &
      slope_path, &
      aspect_path, &
      geo_class_path, &
      soil_class_path, &
      soil_horizon_class_path, &
      lai_class_path, &
      river_width_path, &
      morph_mask_path, &
      pre_var, &
      pet_var, &
      temp_var, &
      tann_var, &
      tmin_var, &
      tmax_var, &
      ssrd_var, &
      strd_var, &
      netrad_var, &
      eabs_var, &
      wind_var, &
      meteo_mask_var, &
      runoff_var, &
      runoff_sealed_var, &
      interflow_fast_var, &
      interflow_slow_var, &
      baseflow_var, &
      hydro_mask_var, &
      dem_var, &
      slope_var, &
      aspect_var, &
      geo_class_var, &
      soil_class_var, &
      lai_class_var, &
      river_width_var, &
      morph_mask_var, &
      hydro_lat_var, &
      hydro_lon_var, &
      morph_lat_var, &
      morph_lon_var, &
      route_lat_var, &
      route_lon_var

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return
    chunking = this%chunking
    time_stamp_location = this%time_stamp_location
    latlon_path = this%latlon_path
    morph_latlon = this%morph_latlon
    pre_path = this%pre_path
    pet_path = this%pet_path
    temp_path = this%temp_path
    tann_path = this%tann_path
    tmin_path = this%tmin_path
    tmax_path = this%tmax_path
    ssrd_path = this%ssrd_path
    strd_path = this%strd_path
    netrad_path = this%netrad_path
    eabs_path = this%eabs_path
    wind_path = this%wind_path
    meteo_mask_path = this%meteo_mask_path
    runoff_path = this%runoff_path
    runoff_sealed_path = this%runoff_sealed_path
    interflow_fast_path = this%interflow_fast_path
    interflow_slow_path = this%interflow_slow_path
    baseflow_path = this%baseflow_path
    hydro_mask_path = this%hydro_mask_path
    dem_path = this%dem_path
    slope_path = this%slope_path
    aspect_path = this%aspect_path
    geo_class_path = this%geo_class_path
    soil_class_path = this%soil_class_path
    soil_horizon_class_path = this%soil_horizon_class_path
    lai_class_path = this%lai_class_path
    river_width_path = this%river_width_path
    morph_mask_path = this%morph_mask_path
    pre_var = this%pre_var
    pet_var = this%pet_var
    temp_var = this%temp_var
    tann_var = this%tann_var
    tmin_var = this%tmin_var
    tmax_var = this%tmax_var
    ssrd_var = this%ssrd_var
    strd_var = this%strd_var
    netrad_var = this%netrad_var
    eabs_var = this%eabs_var
    wind_var = this%wind_var
    meteo_mask_var = this%meteo_mask_var
    runoff_var = this%runoff_var
    runoff_sealed_var = this%runoff_sealed_var
    interflow_fast_var = this%interflow_fast_var
    interflow_slow_var = this%interflow_slow_var
    baseflow_var = this%baseflow_var
    hydro_mask_var = this%hydro_mask_var
    dem_var = this%dem_var
    slope_var = this%slope_var
    aspect_var = this%aspect_var
    geo_class_var = this%geo_class_var
    soil_class_var = this%soil_class_var
    lai_class_var = this%lai_class_var
    river_width_var = this%river_width_var
    morph_mask_var = this%morph_mask_var
    hydro_lat_var = this%hydro_lat_var
    hydro_lon_var = this%hydro_lon_var
    morph_lat_var = this%morph_lat_var
    morph_lon_var = this%morph_lon_var
    route_lat_var = this%route_lat_var
    route_lon_var = this%route_lon_var

    status = nml%open(file, errmsg=errmsg)
    if (status /= NML_OK) return

    status = nml%find("config_input", errmsg=errmsg)
    if (status /= NML_OK) then
      close_status = nml%close()
      return
    end if

    ! read namelist
    read(nml%unit, nml=config_input, iostat=iostat, iomsg=iomsg)
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
    this%chunking = chunking
    this%time_stamp_location = time_stamp_location
    this%latlon_path = latlon_path
    this%morph_latlon = morph_latlon
    this%pre_path = pre_path
    this%pet_path = pet_path
    this%temp_path = temp_path
    this%tann_path = tann_path
    this%tmin_path = tmin_path
    this%tmax_path = tmax_path
    this%ssrd_path = ssrd_path
    this%strd_path = strd_path
    this%netrad_path = netrad_path
    this%eabs_path = eabs_path
    this%wind_path = wind_path
    this%meteo_mask_path = meteo_mask_path
    this%runoff_path = runoff_path
    this%runoff_sealed_path = runoff_sealed_path
    this%interflow_fast_path = interflow_fast_path
    this%interflow_slow_path = interflow_slow_path
    this%baseflow_path = baseflow_path
    this%hydro_mask_path = hydro_mask_path
    this%dem_path = dem_path
    this%slope_path = slope_path
    this%aspect_path = aspect_path
    this%geo_class_path = geo_class_path
    this%soil_class_path = soil_class_path
    this%soil_horizon_class_path = soil_horizon_class_path
    this%lai_class_path = lai_class_path
    this%river_width_path = river_width_path
    this%morph_mask_path = morph_mask_path
    this%pre_var = pre_var
    this%pet_var = pet_var
    this%temp_var = temp_var
    this%tann_var = tann_var
    this%tmin_var = tmin_var
    this%tmax_var = tmax_var
    this%ssrd_var = ssrd_var
    this%strd_var = strd_var
    this%netrad_var = netrad_var
    this%eabs_var = eabs_var
    this%wind_var = wind_var
    this%meteo_mask_var = meteo_mask_var
    this%runoff_var = runoff_var
    this%runoff_sealed_var = runoff_sealed_var
    this%interflow_fast_var = interflow_fast_var
    this%interflow_slow_var = interflow_slow_var
    this%baseflow_var = baseflow_var
    this%hydro_mask_var = hydro_mask_var
    this%dem_var = dem_var
    this%slope_var = slope_var
    this%aspect_var = aspect_var
    this%geo_class_var = geo_class_var
    this%soil_class_var = soil_class_var
    this%lai_class_var = lai_class_var
    this%river_width_var = river_width_var
    this%morph_mask_var = morph_mask_var
    this%hydro_lat_var = hydro_lat_var
    this%hydro_lon_var = hydro_lon_var
    this%morph_lat_var = morph_lat_var
    this%morph_lon_var = morph_lon_var
    this%route_lat_var = route_lat_var
    this%route_lon_var = route_lon_var

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_config_input_from_file

  !> \brief Set config_input values
  integer function nml_config_input_set(this, &
    chunking, &
    time_stamp_location, &
    latlon_path, &
    morph_latlon, &
    pre_path, &
    pet_path, &
    temp_path, &
    tann_path, &
    tmin_path, &
    tmax_path, &
    ssrd_path, &
    strd_path, &
    netrad_path, &
    eabs_path, &
    wind_path, &
    meteo_mask_path, &
    runoff_path, &
    runoff_sealed_path, &
    interflow_fast_path, &
    interflow_slow_path, &
    baseflow_path, &
    hydro_mask_path, &
    dem_path, &
    slope_path, &
    aspect_path, &
    geo_class_path, &
    soil_class_path, &
    soil_horizon_class_path, &
    lai_class_path, &
    river_width_path, &
    morph_mask_path, &
    pre_var, &
    pet_var, &
    temp_var, &
    tann_var, &
    tmin_var, &
    tmax_var, &
    ssrd_var, &
    strd_var, &
    netrad_var, &
    eabs_var, &
    wind_var, &
    meteo_mask_var, &
    runoff_var, &
    runoff_sealed_var, &
    interflow_fast_var, &
    interflow_slow_var, &
    baseflow_var, &
    hydro_mask_var, &
    dem_var, &
    slope_var, &
    aspect_var, &
    geo_class_var, &
    soil_class_var, &
    lai_class_var, &
    river_width_var, &
    morph_mask_var, &
    hydro_lat_var, &
    hydro_lon_var, &
    morph_lat_var, &
    morph_lon_var, &
    route_lat_var, &
    route_lon_var, &
    errmsg) result(status)

    class(nml_config_input_t), intent(inout) :: this
    character(len=*), intent(out), optional :: errmsg
    integer(i4), dimension(:), intent(in), optional :: chunking
    integer(i4), dimension(:), intent(in), optional :: time_stamp_location
    character(len=*), dimension(:), intent(in), optional :: latlon_path
    logical, dimension(:), intent(in), optional :: morph_latlon
    character(len=*), dimension(:), intent(in), optional :: pre_path
    character(len=*), dimension(:), intent(in), optional :: pet_path
    character(len=*), dimension(:), intent(in), optional :: temp_path
    character(len=*), dimension(:), intent(in), optional :: tann_path
    character(len=*), dimension(:), intent(in), optional :: tmin_path
    character(len=*), dimension(:), intent(in), optional :: tmax_path
    character(len=*), dimension(:), intent(in), optional :: ssrd_path
    character(len=*), dimension(:), intent(in), optional :: strd_path
    character(len=*), dimension(:), intent(in), optional :: netrad_path
    character(len=*), dimension(:), intent(in), optional :: eabs_path
    character(len=*), dimension(:), intent(in), optional :: wind_path
    character(len=*), dimension(:), intent(in), optional :: meteo_mask_path
    character(len=*), dimension(:), intent(in), optional :: runoff_path
    character(len=*), dimension(:), intent(in), optional :: runoff_sealed_path
    character(len=*), dimension(:), intent(in), optional :: interflow_fast_path
    character(len=*), dimension(:), intent(in), optional :: interflow_slow_path
    character(len=*), dimension(:), intent(in), optional :: baseflow_path
    character(len=*), dimension(:), intent(in), optional :: hydro_mask_path
    character(len=*), dimension(:), intent(in), optional :: dem_path
    character(len=*), dimension(:), intent(in), optional :: slope_path
    character(len=*), dimension(:), intent(in), optional :: aspect_path
    character(len=*), dimension(:), intent(in), optional :: geo_class_path
    character(len=*), dimension(:), intent(in), optional :: soil_class_path
    character(len=*), dimension(:), intent(in), optional :: soil_horizon_class_path
    character(len=*), dimension(:), intent(in), optional :: lai_class_path
    character(len=*), dimension(:), intent(in), optional :: river_width_path
    character(len=*), dimension(:), intent(in), optional :: morph_mask_path
    character(len=*), dimension(:), intent(in), optional :: pre_var
    character(len=*), dimension(:), intent(in), optional :: pet_var
    character(len=*), dimension(:), intent(in), optional :: temp_var
    character(len=*), dimension(:), intent(in), optional :: tann_var
    character(len=*), dimension(:), intent(in), optional :: tmin_var
    character(len=*), dimension(:), intent(in), optional :: tmax_var
    character(len=*), dimension(:), intent(in), optional :: ssrd_var
    character(len=*), dimension(:), intent(in), optional :: strd_var
    character(len=*), dimension(:), intent(in), optional :: netrad_var
    character(len=*), dimension(:), intent(in), optional :: eabs_var
    character(len=*), dimension(:), intent(in), optional :: wind_var
    character(len=*), dimension(:), intent(in), optional :: meteo_mask_var
    character(len=*), dimension(:), intent(in), optional :: runoff_var
    character(len=*), dimension(:), intent(in), optional :: runoff_sealed_var
    character(len=*), dimension(:), intent(in), optional :: interflow_fast_var
    character(len=*), dimension(:), intent(in), optional :: interflow_slow_var
    character(len=*), dimension(:), intent(in), optional :: baseflow_var
    character(len=*), dimension(:), intent(in), optional :: hydro_mask_var
    character(len=*), dimension(:), intent(in), optional :: dem_var
    character(len=*), dimension(:), intent(in), optional :: slope_var
    character(len=*), dimension(:), intent(in), optional :: aspect_var
    character(len=*), dimension(:), intent(in), optional :: geo_class_var
    character(len=*), dimension(:), intent(in), optional :: soil_class_var
    character(len=*), dimension(:), intent(in), optional :: lai_class_var
    character(len=*), dimension(:), intent(in), optional :: river_width_var
    character(len=*), dimension(:), intent(in), optional :: morph_mask_var
    character(len=*), dimension(:), intent(in), optional :: hydro_lat_var
    character(len=*), dimension(:), intent(in), optional :: hydro_lon_var
    character(len=*), dimension(:), intent(in), optional :: morph_lat_var
    character(len=*), dimension(:), intent(in), optional :: morph_lon_var
    character(len=*), dimension(:), intent(in), optional :: route_lat_var
    character(len=*), dimension(:), intent(in), optional :: route_lon_var
    integer :: &
      lb_1, &
      ub_1

    status = this%init(errmsg=errmsg)
    if (status /= NML_OK) return

    ! required parameters
    ! override with provided values
    if (present(chunking)) then
      if (size(chunking, 1) > size(this%chunking, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'chunking'"
        return
      end if
      lb_1 = lbound(this%chunking, 1)
      ub_1 = lb_1 + size(chunking, 1) - 1
      this%chunking(lb_1:ub_1) = chunking
    end if
    if (present(time_stamp_location)) then
      if (size(time_stamp_location, 1) > size(this%time_stamp_location, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'time_stamp_location'"
        return
      end if
      lb_1 = lbound(this%time_stamp_location, 1)
      ub_1 = lb_1 + size(time_stamp_location, 1) - 1
      this%time_stamp_location(lb_1:ub_1) = time_stamp_location
    end if
    if (present(latlon_path)) then
      if (size(latlon_path, 1) > size(this%latlon_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'latlon_path'"
        return
      end if
      lb_1 = lbound(this%latlon_path, 1)
      ub_1 = lb_1 + size(latlon_path, 1) - 1
      this%latlon_path(lb_1:ub_1) = latlon_path
    end if
    if (present(morph_latlon)) then
      if (size(morph_latlon, 1) > size(this%morph_latlon, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'morph_latlon'"
        return
      end if
      lb_1 = lbound(this%morph_latlon, 1)
      ub_1 = lb_1 + size(morph_latlon, 1) - 1
      this%morph_latlon(lb_1:ub_1) = morph_latlon
    end if
    if (present(pre_path)) then
      if (size(pre_path, 1) > size(this%pre_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'pre_path'"
        return
      end if
      lb_1 = lbound(this%pre_path, 1)
      ub_1 = lb_1 + size(pre_path, 1) - 1
      this%pre_path(lb_1:ub_1) = pre_path
    end if
    if (present(pet_path)) then
      if (size(pet_path, 1) > size(this%pet_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'pet_path'"
        return
      end if
      lb_1 = lbound(this%pet_path, 1)
      ub_1 = lb_1 + size(pet_path, 1) - 1
      this%pet_path(lb_1:ub_1) = pet_path
    end if
    if (present(temp_path)) then
      if (size(temp_path, 1) > size(this%temp_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'temp_path'"
        return
      end if
      lb_1 = lbound(this%temp_path, 1)
      ub_1 = lb_1 + size(temp_path, 1) - 1
      this%temp_path(lb_1:ub_1) = temp_path
    end if
    if (present(tann_path)) then
      if (size(tann_path, 1) > size(this%tann_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'tann_path'"
        return
      end if
      lb_1 = lbound(this%tann_path, 1)
      ub_1 = lb_1 + size(tann_path, 1) - 1
      this%tann_path(lb_1:ub_1) = tann_path
    end if
    if (present(tmin_path)) then
      if (size(tmin_path, 1) > size(this%tmin_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'tmin_path'"
        return
      end if
      lb_1 = lbound(this%tmin_path, 1)
      ub_1 = lb_1 + size(tmin_path, 1) - 1
      this%tmin_path(lb_1:ub_1) = tmin_path
    end if
    if (present(tmax_path)) then
      if (size(tmax_path, 1) > size(this%tmax_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'tmax_path'"
        return
      end if
      lb_1 = lbound(this%tmax_path, 1)
      ub_1 = lb_1 + size(tmax_path, 1) - 1
      this%tmax_path(lb_1:ub_1) = tmax_path
    end if
    if (present(ssrd_path)) then
      if (size(ssrd_path, 1) > size(this%ssrd_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'ssrd_path'"
        return
      end if
      lb_1 = lbound(this%ssrd_path, 1)
      ub_1 = lb_1 + size(ssrd_path, 1) - 1
      this%ssrd_path(lb_1:ub_1) = ssrd_path
    end if
    if (present(strd_path)) then
      if (size(strd_path, 1) > size(this%strd_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'strd_path'"
        return
      end if
      lb_1 = lbound(this%strd_path, 1)
      ub_1 = lb_1 + size(strd_path, 1) - 1
      this%strd_path(lb_1:ub_1) = strd_path
    end if
    if (present(netrad_path)) then
      if (size(netrad_path, 1) > size(this%netrad_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'netrad_path'"
        return
      end if
      lb_1 = lbound(this%netrad_path, 1)
      ub_1 = lb_1 + size(netrad_path, 1) - 1
      this%netrad_path(lb_1:ub_1) = netrad_path
    end if
    if (present(eabs_path)) then
      if (size(eabs_path, 1) > size(this%eabs_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'eabs_path'"
        return
      end if
      lb_1 = lbound(this%eabs_path, 1)
      ub_1 = lb_1 + size(eabs_path, 1) - 1
      this%eabs_path(lb_1:ub_1) = eabs_path
    end if
    if (present(wind_path)) then
      if (size(wind_path, 1) > size(this%wind_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'wind_path'"
        return
      end if
      lb_1 = lbound(this%wind_path, 1)
      ub_1 = lb_1 + size(wind_path, 1) - 1
      this%wind_path(lb_1:ub_1) = wind_path
    end if
    if (present(meteo_mask_path)) then
      if (size(meteo_mask_path, 1) > size(this%meteo_mask_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'meteo_mask_path'"
        return
      end if
      lb_1 = lbound(this%meteo_mask_path, 1)
      ub_1 = lb_1 + size(meteo_mask_path, 1) - 1
      this%meteo_mask_path(lb_1:ub_1) = meteo_mask_path
    end if
    if (present(runoff_path)) then
      if (size(runoff_path, 1) > size(this%runoff_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'runoff_path'"
        return
      end if
      lb_1 = lbound(this%runoff_path, 1)
      ub_1 = lb_1 + size(runoff_path, 1) - 1
      this%runoff_path(lb_1:ub_1) = runoff_path
    end if
    if (present(runoff_sealed_path)) then
      if (size(runoff_sealed_path, 1) > size(this%runoff_sealed_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'runoff_sealed_path'"
        return
      end if
      lb_1 = lbound(this%runoff_sealed_path, 1)
      ub_1 = lb_1 + size(runoff_sealed_path, 1) - 1
      this%runoff_sealed_path(lb_1:ub_1) = runoff_sealed_path
    end if
    if (present(interflow_fast_path)) then
      if (size(interflow_fast_path, 1) > size(this%interflow_fast_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'interflow_fast_path'"
        return
      end if
      lb_1 = lbound(this%interflow_fast_path, 1)
      ub_1 = lb_1 + size(interflow_fast_path, 1) - 1
      this%interflow_fast_path(lb_1:ub_1) = interflow_fast_path
    end if
    if (present(interflow_slow_path)) then
      if (size(interflow_slow_path, 1) > size(this%interflow_slow_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'interflow_slow_path'"
        return
      end if
      lb_1 = lbound(this%interflow_slow_path, 1)
      ub_1 = lb_1 + size(interflow_slow_path, 1) - 1
      this%interflow_slow_path(lb_1:ub_1) = interflow_slow_path
    end if
    if (present(baseflow_path)) then
      if (size(baseflow_path, 1) > size(this%baseflow_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'baseflow_path'"
        return
      end if
      lb_1 = lbound(this%baseflow_path, 1)
      ub_1 = lb_1 + size(baseflow_path, 1) - 1
      this%baseflow_path(lb_1:ub_1) = baseflow_path
    end if
    if (present(hydro_mask_path)) then
      if (size(hydro_mask_path, 1) > size(this%hydro_mask_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'hydro_mask_path'"
        return
      end if
      lb_1 = lbound(this%hydro_mask_path, 1)
      ub_1 = lb_1 + size(hydro_mask_path, 1) - 1
      this%hydro_mask_path(lb_1:ub_1) = hydro_mask_path
    end if
    if (present(dem_path)) then
      if (size(dem_path, 1) > size(this%dem_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'dem_path'"
        return
      end if
      lb_1 = lbound(this%dem_path, 1)
      ub_1 = lb_1 + size(dem_path, 1) - 1
      this%dem_path(lb_1:ub_1) = dem_path
    end if
    if (present(slope_path)) then
      if (size(slope_path, 1) > size(this%slope_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'slope_path'"
        return
      end if
      lb_1 = lbound(this%slope_path, 1)
      ub_1 = lb_1 + size(slope_path, 1) - 1
      this%slope_path(lb_1:ub_1) = slope_path
    end if
    if (present(aspect_path)) then
      if (size(aspect_path, 1) > size(this%aspect_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'aspect_path'"
        return
      end if
      lb_1 = lbound(this%aspect_path, 1)
      ub_1 = lb_1 + size(aspect_path, 1) - 1
      this%aspect_path(lb_1:ub_1) = aspect_path
    end if
    if (present(geo_class_path)) then
      if (size(geo_class_path, 1) > size(this%geo_class_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'geo_class_path'"
        return
      end if
      lb_1 = lbound(this%geo_class_path, 1)
      ub_1 = lb_1 + size(geo_class_path, 1) - 1
      this%geo_class_path(lb_1:ub_1) = geo_class_path
    end if
    if (present(soil_class_path)) then
      if (size(soil_class_path, 1) > size(this%soil_class_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'soil_class_path'"
        return
      end if
      lb_1 = lbound(this%soil_class_path, 1)
      ub_1 = lb_1 + size(soil_class_path, 1) - 1
      this%soil_class_path(lb_1:ub_1) = soil_class_path
    end if
    if (present(soil_horizon_class_path)) then
      if (size(soil_horizon_class_path, 1) > size(this%soil_horizon_class_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'soil_horizon_class_path'"
        return
      end if
      lb_1 = lbound(this%soil_horizon_class_path, 1)
      ub_1 = lb_1 + size(soil_horizon_class_path, 1) - 1
      this%soil_horizon_class_path(lb_1:ub_1) = soil_horizon_class_path
    end if
    if (present(lai_class_path)) then
      if (size(lai_class_path, 1) > size(this%lai_class_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'lai_class_path'"
        return
      end if
      lb_1 = lbound(this%lai_class_path, 1)
      ub_1 = lb_1 + size(lai_class_path, 1) - 1
      this%lai_class_path(lb_1:ub_1) = lai_class_path
    end if
    if (present(river_width_path)) then
      if (size(river_width_path, 1) > size(this%river_width_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'river_width_path'"
        return
      end if
      lb_1 = lbound(this%river_width_path, 1)
      ub_1 = lb_1 + size(river_width_path, 1) - 1
      this%river_width_path(lb_1:ub_1) = river_width_path
    end if
    if (present(morph_mask_path)) then
      if (size(morph_mask_path, 1) > size(this%morph_mask_path, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'morph_mask_path'"
        return
      end if
      lb_1 = lbound(this%morph_mask_path, 1)
      ub_1 = lb_1 + size(morph_mask_path, 1) - 1
      this%morph_mask_path(lb_1:ub_1) = morph_mask_path
    end if
    if (present(pre_var)) then
      if (size(pre_var, 1) > size(this%pre_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'pre_var'"
        return
      end if
      lb_1 = lbound(this%pre_var, 1)
      ub_1 = lb_1 + size(pre_var, 1) - 1
      this%pre_var(lb_1:ub_1) = pre_var
    end if
    if (present(pet_var)) then
      if (size(pet_var, 1) > size(this%pet_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'pet_var'"
        return
      end if
      lb_1 = lbound(this%pet_var, 1)
      ub_1 = lb_1 + size(pet_var, 1) - 1
      this%pet_var(lb_1:ub_1) = pet_var
    end if
    if (present(temp_var)) then
      if (size(temp_var, 1) > size(this%temp_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'temp_var'"
        return
      end if
      lb_1 = lbound(this%temp_var, 1)
      ub_1 = lb_1 + size(temp_var, 1) - 1
      this%temp_var(lb_1:ub_1) = temp_var
    end if
    if (present(tann_var)) then
      if (size(tann_var, 1) > size(this%tann_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'tann_var'"
        return
      end if
      lb_1 = lbound(this%tann_var, 1)
      ub_1 = lb_1 + size(tann_var, 1) - 1
      this%tann_var(lb_1:ub_1) = tann_var
    end if
    if (present(tmin_var)) then
      if (size(tmin_var, 1) > size(this%tmin_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'tmin_var'"
        return
      end if
      lb_1 = lbound(this%tmin_var, 1)
      ub_1 = lb_1 + size(tmin_var, 1) - 1
      this%tmin_var(lb_1:ub_1) = tmin_var
    end if
    if (present(tmax_var)) then
      if (size(tmax_var, 1) > size(this%tmax_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'tmax_var'"
        return
      end if
      lb_1 = lbound(this%tmax_var, 1)
      ub_1 = lb_1 + size(tmax_var, 1) - 1
      this%tmax_var(lb_1:ub_1) = tmax_var
    end if
    if (present(ssrd_var)) then
      if (size(ssrd_var, 1) > size(this%ssrd_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'ssrd_var'"
        return
      end if
      lb_1 = lbound(this%ssrd_var, 1)
      ub_1 = lb_1 + size(ssrd_var, 1) - 1
      this%ssrd_var(lb_1:ub_1) = ssrd_var
    end if
    if (present(strd_var)) then
      if (size(strd_var, 1) > size(this%strd_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'strd_var'"
        return
      end if
      lb_1 = lbound(this%strd_var, 1)
      ub_1 = lb_1 + size(strd_var, 1) - 1
      this%strd_var(lb_1:ub_1) = strd_var
    end if
    if (present(netrad_var)) then
      if (size(netrad_var, 1) > size(this%netrad_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'netrad_var'"
        return
      end if
      lb_1 = lbound(this%netrad_var, 1)
      ub_1 = lb_1 + size(netrad_var, 1) - 1
      this%netrad_var(lb_1:ub_1) = netrad_var
    end if
    if (present(eabs_var)) then
      if (size(eabs_var, 1) > size(this%eabs_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'eabs_var'"
        return
      end if
      lb_1 = lbound(this%eabs_var, 1)
      ub_1 = lb_1 + size(eabs_var, 1) - 1
      this%eabs_var(lb_1:ub_1) = eabs_var
    end if
    if (present(wind_var)) then
      if (size(wind_var, 1) > size(this%wind_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'wind_var'"
        return
      end if
      lb_1 = lbound(this%wind_var, 1)
      ub_1 = lb_1 + size(wind_var, 1) - 1
      this%wind_var(lb_1:ub_1) = wind_var
    end if
    if (present(meteo_mask_var)) then
      if (size(meteo_mask_var, 1) > size(this%meteo_mask_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'meteo_mask_var'"
        return
      end if
      lb_1 = lbound(this%meteo_mask_var, 1)
      ub_1 = lb_1 + size(meteo_mask_var, 1) - 1
      this%meteo_mask_var(lb_1:ub_1) = meteo_mask_var
    end if
    if (present(runoff_var)) then
      if (size(runoff_var, 1) > size(this%runoff_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'runoff_var'"
        return
      end if
      lb_1 = lbound(this%runoff_var, 1)
      ub_1 = lb_1 + size(runoff_var, 1) - 1
      this%runoff_var(lb_1:ub_1) = runoff_var
    end if
    if (present(runoff_sealed_var)) then
      if (size(runoff_sealed_var, 1) > size(this%runoff_sealed_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'runoff_sealed_var'"
        return
      end if
      lb_1 = lbound(this%runoff_sealed_var, 1)
      ub_1 = lb_1 + size(runoff_sealed_var, 1) - 1
      this%runoff_sealed_var(lb_1:ub_1) = runoff_sealed_var
    end if
    if (present(interflow_fast_var)) then
      if (size(interflow_fast_var, 1) > size(this%interflow_fast_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'interflow_fast_var'"
        return
      end if
      lb_1 = lbound(this%interflow_fast_var, 1)
      ub_1 = lb_1 + size(interflow_fast_var, 1) - 1
      this%interflow_fast_var(lb_1:ub_1) = interflow_fast_var
    end if
    if (present(interflow_slow_var)) then
      if (size(interflow_slow_var, 1) > size(this%interflow_slow_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'interflow_slow_var'"
        return
      end if
      lb_1 = lbound(this%interflow_slow_var, 1)
      ub_1 = lb_1 + size(interflow_slow_var, 1) - 1
      this%interflow_slow_var(lb_1:ub_1) = interflow_slow_var
    end if
    if (present(baseflow_var)) then
      if (size(baseflow_var, 1) > size(this%baseflow_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'baseflow_var'"
        return
      end if
      lb_1 = lbound(this%baseflow_var, 1)
      ub_1 = lb_1 + size(baseflow_var, 1) - 1
      this%baseflow_var(lb_1:ub_1) = baseflow_var
    end if
    if (present(hydro_mask_var)) then
      if (size(hydro_mask_var, 1) > size(this%hydro_mask_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'hydro_mask_var'"
        return
      end if
      lb_1 = lbound(this%hydro_mask_var, 1)
      ub_1 = lb_1 + size(hydro_mask_var, 1) - 1
      this%hydro_mask_var(lb_1:ub_1) = hydro_mask_var
    end if
    if (present(dem_var)) then
      if (size(dem_var, 1) > size(this%dem_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'dem_var'"
        return
      end if
      lb_1 = lbound(this%dem_var, 1)
      ub_1 = lb_1 + size(dem_var, 1) - 1
      this%dem_var(lb_1:ub_1) = dem_var
    end if
    if (present(slope_var)) then
      if (size(slope_var, 1) > size(this%slope_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'slope_var'"
        return
      end if
      lb_1 = lbound(this%slope_var, 1)
      ub_1 = lb_1 + size(slope_var, 1) - 1
      this%slope_var(lb_1:ub_1) = slope_var
    end if
    if (present(aspect_var)) then
      if (size(aspect_var, 1) > size(this%aspect_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'aspect_var'"
        return
      end if
      lb_1 = lbound(this%aspect_var, 1)
      ub_1 = lb_1 + size(aspect_var, 1) - 1
      this%aspect_var(lb_1:ub_1) = aspect_var
    end if
    if (present(geo_class_var)) then
      if (size(geo_class_var, 1) > size(this%geo_class_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'geo_class_var'"
        return
      end if
      lb_1 = lbound(this%geo_class_var, 1)
      ub_1 = lb_1 + size(geo_class_var, 1) - 1
      this%geo_class_var(lb_1:ub_1) = geo_class_var
    end if
    if (present(soil_class_var)) then
      if (size(soil_class_var, 1) > size(this%soil_class_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'soil_class_var'"
        return
      end if
      lb_1 = lbound(this%soil_class_var, 1)
      ub_1 = lb_1 + size(soil_class_var, 1) - 1
      this%soil_class_var(lb_1:ub_1) = soil_class_var
    end if
    if (present(lai_class_var)) then
      if (size(lai_class_var, 1) > size(this%lai_class_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'lai_class_var'"
        return
      end if
      lb_1 = lbound(this%lai_class_var, 1)
      ub_1 = lb_1 + size(lai_class_var, 1) - 1
      this%lai_class_var(lb_1:ub_1) = lai_class_var
    end if
    if (present(river_width_var)) then
      if (size(river_width_var, 1) > size(this%river_width_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'river_width_var'"
        return
      end if
      lb_1 = lbound(this%river_width_var, 1)
      ub_1 = lb_1 + size(river_width_var, 1) - 1
      this%river_width_var(lb_1:ub_1) = river_width_var
    end if
    if (present(morph_mask_var)) then
      if (size(morph_mask_var, 1) > size(this%morph_mask_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'morph_mask_var'"
        return
      end if
      lb_1 = lbound(this%morph_mask_var, 1)
      ub_1 = lb_1 + size(morph_mask_var, 1) - 1
      this%morph_mask_var(lb_1:ub_1) = morph_mask_var
    end if
    if (present(hydro_lat_var)) then
      if (size(hydro_lat_var, 1) > size(this%hydro_lat_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'hydro_lat_var'"
        return
      end if
      lb_1 = lbound(this%hydro_lat_var, 1)
      ub_1 = lb_1 + size(hydro_lat_var, 1) - 1
      this%hydro_lat_var(lb_1:ub_1) = hydro_lat_var
    end if
    if (present(hydro_lon_var)) then
      if (size(hydro_lon_var, 1) > size(this%hydro_lon_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'hydro_lon_var'"
        return
      end if
      lb_1 = lbound(this%hydro_lon_var, 1)
      ub_1 = lb_1 + size(hydro_lon_var, 1) - 1
      this%hydro_lon_var(lb_1:ub_1) = hydro_lon_var
    end if
    if (present(morph_lat_var)) then
      if (size(morph_lat_var, 1) > size(this%morph_lat_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'morph_lat_var'"
        return
      end if
      lb_1 = lbound(this%morph_lat_var, 1)
      ub_1 = lb_1 + size(morph_lat_var, 1) - 1
      this%morph_lat_var(lb_1:ub_1) = morph_lat_var
    end if
    if (present(morph_lon_var)) then
      if (size(morph_lon_var, 1) > size(this%morph_lon_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'morph_lon_var'"
        return
      end if
      lb_1 = lbound(this%morph_lon_var, 1)
      ub_1 = lb_1 + size(morph_lon_var, 1) - 1
      this%morph_lon_var(lb_1:ub_1) = morph_lon_var
    end if
    if (present(route_lat_var)) then
      if (size(route_lat_var, 1) > size(this%route_lat_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'route_lat_var'"
        return
      end if
      lb_1 = lbound(this%route_lat_var, 1)
      ub_1 = lb_1 + size(route_lat_var, 1) - 1
      this%route_lat_var(lb_1:ub_1) = route_lat_var
    end if
    if (present(route_lon_var)) then
      if (size(route_lon_var, 1) > size(this%route_lon_var, 1)) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "dimension 1 exceeds bounds for 'route_lon_var'"
        return
      end if
      lb_1 = lbound(this%route_lon_var, 1)
      ub_1 = lb_1 + size(route_lon_var, 1) - 1
      this%route_lon_var(lb_1:ub_1) = route_lon_var
    end if

    ! mark as configured
    this%is_configured = .true.
    status = NML_OK
  end function nml_config_input_set

  !> \brief Check whether a namelist value was set
  integer function nml_config_input_is_set(this, name, idx, errmsg) result(status)
    class(nml_config_input_t), intent(in) :: this
    character(len=*), intent(in) :: name
    integer, intent(in), optional :: idx(:)
    character(len=*), intent(out), optional :: errmsg

    status = NML_OK
    if (present(errmsg)) errmsg = ""
    select case (trim(name))
    case ("chunking")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%chunking), ubound(this%chunking), &
          "chunking", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("time_stamp_location")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%time_stamp_location), ubound(this%time_stamp_location), &
          "time_stamp_location", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("latlon_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%latlon_path), ubound(this%latlon_path), &
          "latlon_path", errmsg)
        if (status /= NML_OK) return
        if (this%latlon_path(idx(1)) == repeat(achar(0), len(this%latlon_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%latlon_path == repeat(achar(0), len(this%latlon_path)))) status = NML_ERR_NOT_SET
      end if
    case ("morph_latlon")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%morph_latlon), ubound(this%morph_latlon), &
          "morph_latlon", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("pre_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%pre_path), ubound(this%pre_path), &
          "pre_path", errmsg)
        if (status /= NML_OK) return
        if (this%pre_path(idx(1)) == repeat(achar(0), len(this%pre_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%pre_path == repeat(achar(0), len(this%pre_path)))) status = NML_ERR_NOT_SET
      end if
    case ("pet_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%pet_path), ubound(this%pet_path), &
          "pet_path", errmsg)
        if (status /= NML_OK) return
        if (this%pet_path(idx(1)) == repeat(achar(0), len(this%pet_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%pet_path == repeat(achar(0), len(this%pet_path)))) status = NML_ERR_NOT_SET
      end if
    case ("temp_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%temp_path), ubound(this%temp_path), &
          "temp_path", errmsg)
        if (status /= NML_OK) return
        if (this%temp_path(idx(1)) == repeat(achar(0), len(this%temp_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%temp_path == repeat(achar(0), len(this%temp_path)))) status = NML_ERR_NOT_SET
      end if
    case ("tann_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%tann_path), ubound(this%tann_path), &
          "tann_path", errmsg)
        if (status /= NML_OK) return
        if (this%tann_path(idx(1)) == repeat(achar(0), len(this%tann_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%tann_path == repeat(achar(0), len(this%tann_path)))) status = NML_ERR_NOT_SET
      end if
    case ("tmin_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%tmin_path), ubound(this%tmin_path), &
          "tmin_path", errmsg)
        if (status /= NML_OK) return
        if (this%tmin_path(idx(1)) == repeat(achar(0), len(this%tmin_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%tmin_path == repeat(achar(0), len(this%tmin_path)))) status = NML_ERR_NOT_SET
      end if
    case ("tmax_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%tmax_path), ubound(this%tmax_path), &
          "tmax_path", errmsg)
        if (status /= NML_OK) return
        if (this%tmax_path(idx(1)) == repeat(achar(0), len(this%tmax_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%tmax_path == repeat(achar(0), len(this%tmax_path)))) status = NML_ERR_NOT_SET
      end if
    case ("ssrd_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%ssrd_path), ubound(this%ssrd_path), &
          "ssrd_path", errmsg)
        if (status /= NML_OK) return
        if (this%ssrd_path(idx(1)) == repeat(achar(0), len(this%ssrd_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%ssrd_path == repeat(achar(0), len(this%ssrd_path)))) status = NML_ERR_NOT_SET
      end if
    case ("strd_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%strd_path), ubound(this%strd_path), &
          "strd_path", errmsg)
        if (status /= NML_OK) return
        if (this%strd_path(idx(1)) == repeat(achar(0), len(this%strd_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%strd_path == repeat(achar(0), len(this%strd_path)))) status = NML_ERR_NOT_SET
      end if
    case ("netrad_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%netrad_path), ubound(this%netrad_path), &
          "netrad_path", errmsg)
        if (status /= NML_OK) return
        if (this%netrad_path(idx(1)) == repeat(achar(0), len(this%netrad_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%netrad_path == repeat(achar(0), len(this%netrad_path)))) status = NML_ERR_NOT_SET
      end if
    case ("eabs_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%eabs_path), ubound(this%eabs_path), &
          "eabs_path", errmsg)
        if (status /= NML_OK) return
        if (this%eabs_path(idx(1)) == repeat(achar(0), len(this%eabs_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%eabs_path == repeat(achar(0), len(this%eabs_path)))) status = NML_ERR_NOT_SET
      end if
    case ("wind_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%wind_path), ubound(this%wind_path), &
          "wind_path", errmsg)
        if (status /= NML_OK) return
        if (this%wind_path(idx(1)) == repeat(achar(0), len(this%wind_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%wind_path == repeat(achar(0), len(this%wind_path)))) status = NML_ERR_NOT_SET
      end if
    case ("meteo_mask_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%meteo_mask_path), ubound(this%meteo_mask_path), &
          "meteo_mask_path", errmsg)
        if (status /= NML_OK) return
        if (this%meteo_mask_path(idx(1)) == repeat(achar(0), len(this%meteo_mask_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%meteo_mask_path == repeat(achar(0), len(this%meteo_mask_path)))) status = NML_ERR_NOT_SET
      end if
    case ("runoff_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%runoff_path), ubound(this%runoff_path), &
          "runoff_path", errmsg)
        if (status /= NML_OK) return
        if (this%runoff_path(idx(1)) == repeat(achar(0), len(this%runoff_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%runoff_path == repeat(achar(0), len(this%runoff_path)))) status = NML_ERR_NOT_SET
      end if
    case ("runoff_sealed_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%runoff_sealed_path), ubound(this%runoff_sealed_path), &
          "runoff_sealed_path", errmsg)
        if (status /= NML_OK) return
        if (this%runoff_sealed_path(idx(1)) == repeat(achar(0), len(this%runoff_sealed_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%runoff_sealed_path == repeat(achar(0), len(this%runoff_sealed_path)))) status = NML_ERR_NOT_SET
      end if
    case ("interflow_fast_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%interflow_fast_path), ubound(this%interflow_fast_path), &
          "interflow_fast_path", errmsg)
        if (status /= NML_OK) return
        if (this%interflow_fast_path(idx(1)) == repeat(achar(0), len(this%interflow_fast_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%interflow_fast_path == repeat(achar(0), len(this%interflow_fast_path)))) status = NML_ERR_NOT_SET
      end if
    case ("interflow_slow_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%interflow_slow_path), ubound(this%interflow_slow_path), &
          "interflow_slow_path", errmsg)
        if (status /= NML_OK) return
        if (this%interflow_slow_path(idx(1)) == repeat(achar(0), len(this%interflow_slow_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%interflow_slow_path == repeat(achar(0), len(this%interflow_slow_path)))) status = NML_ERR_NOT_SET
      end if
    case ("baseflow_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%baseflow_path), ubound(this%baseflow_path), &
          "baseflow_path", errmsg)
        if (status /= NML_OK) return
        if (this%baseflow_path(idx(1)) == repeat(achar(0), len(this%baseflow_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%baseflow_path == repeat(achar(0), len(this%baseflow_path)))) status = NML_ERR_NOT_SET
      end if
    case ("hydro_mask_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%hydro_mask_path), ubound(this%hydro_mask_path), &
          "hydro_mask_path", errmsg)
        if (status /= NML_OK) return
        if (this%hydro_mask_path(idx(1)) == repeat(achar(0), len(this%hydro_mask_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%hydro_mask_path == repeat(achar(0), len(this%hydro_mask_path)))) status = NML_ERR_NOT_SET
      end if
    case ("dem_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%dem_path), ubound(this%dem_path), &
          "dem_path", errmsg)
        if (status /= NML_OK) return
        if (this%dem_path(idx(1)) == repeat(achar(0), len(this%dem_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%dem_path == repeat(achar(0), len(this%dem_path)))) status = NML_ERR_NOT_SET
      end if
    case ("slope_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%slope_path), ubound(this%slope_path), &
          "slope_path", errmsg)
        if (status /= NML_OK) return
        if (this%slope_path(idx(1)) == repeat(achar(0), len(this%slope_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%slope_path == repeat(achar(0), len(this%slope_path)))) status = NML_ERR_NOT_SET
      end if
    case ("aspect_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%aspect_path), ubound(this%aspect_path), &
          "aspect_path", errmsg)
        if (status /= NML_OK) return
        if (this%aspect_path(idx(1)) == repeat(achar(0), len(this%aspect_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%aspect_path == repeat(achar(0), len(this%aspect_path)))) status = NML_ERR_NOT_SET
      end if
    case ("geo_class_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%geo_class_path), ubound(this%geo_class_path), &
          "geo_class_path", errmsg)
        if (status /= NML_OK) return
        if (this%geo_class_path(idx(1)) == repeat(achar(0), len(this%geo_class_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%geo_class_path == repeat(achar(0), len(this%geo_class_path)))) status = NML_ERR_NOT_SET
      end if
    case ("soil_class_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%soil_class_path), ubound(this%soil_class_path), &
          "soil_class_path", errmsg)
        if (status /= NML_OK) return
        if (this%soil_class_path(idx(1)) == repeat(achar(0), len(this%soil_class_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%soil_class_path == repeat(achar(0), len(this%soil_class_path)))) status = NML_ERR_NOT_SET
      end if
    case ("soil_horizon_class_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%soil_horizon_class_path), ubound(this%soil_horizon_class_path), &
          "soil_horizon_class_path", errmsg)
        if (status /= NML_OK) return
        if (this%soil_horizon_class_path(idx(1)) == repeat(achar(0), len(this%soil_horizon_class_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%soil_horizon_class_path == repeat(achar(0), len(this%soil_horizon_class_path)))) status = NML_ERR_NOT_SET
      end if
    case ("lai_class_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%lai_class_path), ubound(this%lai_class_path), &
          "lai_class_path", errmsg)
        if (status /= NML_OK) return
        if (this%lai_class_path(idx(1)) == repeat(achar(0), len(this%lai_class_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%lai_class_path == repeat(achar(0), len(this%lai_class_path)))) status = NML_ERR_NOT_SET
      end if
    case ("river_width_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%river_width_path), ubound(this%river_width_path), &
          "river_width_path", errmsg)
        if (status /= NML_OK) return
        if (this%river_width_path(idx(1)) == repeat(achar(0), len(this%river_width_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%river_width_path == repeat(achar(0), len(this%river_width_path)))) status = NML_ERR_NOT_SET
      end if
    case ("morph_mask_path")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%morph_mask_path), ubound(this%morph_mask_path), &
          "morph_mask_path", errmsg)
        if (status /= NML_OK) return
        if (this%morph_mask_path(idx(1)) == repeat(achar(0), len(this%morph_mask_path))) status = NML_ERR_NOT_SET
      else
        if (all(this%morph_mask_path == repeat(achar(0), len(this%morph_mask_path)))) status = NML_ERR_NOT_SET
      end if
    case ("pre_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%pre_var), ubound(this%pre_var), &
          "pre_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("pet_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%pet_var), ubound(this%pet_var), &
          "pet_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("temp_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%temp_var), ubound(this%temp_var), &
          "temp_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("tann_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%tann_var), ubound(this%tann_var), &
          "tann_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("tmin_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%tmin_var), ubound(this%tmin_var), &
          "tmin_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("tmax_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%tmax_var), ubound(this%tmax_var), &
          "tmax_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("ssrd_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%ssrd_var), ubound(this%ssrd_var), &
          "ssrd_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("strd_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%strd_var), ubound(this%strd_var), &
          "strd_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("netrad_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%netrad_var), ubound(this%netrad_var), &
          "netrad_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("eabs_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%eabs_var), ubound(this%eabs_var), &
          "eabs_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("wind_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%wind_var), ubound(this%wind_var), &
          "wind_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("meteo_mask_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%meteo_mask_var), ubound(this%meteo_mask_var), &
          "meteo_mask_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("runoff_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%runoff_var), ubound(this%runoff_var), &
          "runoff_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("runoff_sealed_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%runoff_sealed_var), ubound(this%runoff_sealed_var), &
          "runoff_sealed_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("interflow_fast_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%interflow_fast_var), ubound(this%interflow_fast_var), &
          "interflow_fast_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("interflow_slow_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%interflow_slow_var), ubound(this%interflow_slow_var), &
          "interflow_slow_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("baseflow_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%baseflow_var), ubound(this%baseflow_var), &
          "baseflow_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("hydro_mask_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%hydro_mask_var), ubound(this%hydro_mask_var), &
          "hydro_mask_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("dem_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%dem_var), ubound(this%dem_var), &
          "dem_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("slope_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%slope_var), ubound(this%slope_var), &
          "slope_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("aspect_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%aspect_var), ubound(this%aspect_var), &
          "aspect_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("geo_class_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%geo_class_var), ubound(this%geo_class_var), &
          "geo_class_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("soil_class_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%soil_class_var), ubound(this%soil_class_var), &
          "soil_class_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("lai_class_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%lai_class_var), ubound(this%lai_class_var), &
          "lai_class_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("river_width_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%river_width_var), ubound(this%river_width_var), &
          "river_width_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("morph_mask_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%morph_mask_var), ubound(this%morph_mask_var), &
          "morph_mask_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("hydro_lat_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%hydro_lat_var), ubound(this%hydro_lat_var), &
          "hydro_lat_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("hydro_lon_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%hydro_lon_var), ubound(this%hydro_lon_var), &
          "hydro_lon_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("morph_lat_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%morph_lat_var), ubound(this%morph_lat_var), &
          "morph_lat_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("morph_lon_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%morph_lon_var), ubound(this%morph_lon_var), &
          "morph_lon_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("route_lat_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%route_lat_var), ubound(this%route_lat_var), &
          "route_lat_var", errmsg)
        if (status /= NML_OK) return
      else
      end if
    case ("route_lon_var")
      if (present(idx)) then
        status = idx_check(idx, lbound(this%route_lon_var), ubound(this%route_lon_var), &
          "route_lon_var", errmsg)
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
  end function nml_config_input_is_set

  !> \brief Determine the filled shape along flexible dimensions
  integer function nml_config_input_filled_shape(this, name, filled, errmsg) result(status)
    class(nml_config_input_t), intent(in) :: this
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
    case ("latlon_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'latlon_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%latlon_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%latlon_path, 1), &
        lbound(this%latlon_path, 1), -1
        if (.not. (this%latlon_path(idx) == repeat(achar(0), len(this%latlon_path)))) then
          filled(1) = idx - lbound(this%latlon_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%latlon_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%latlon_path(lb_1:ub_1) == repeat(achar(0), len(this%latlon_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: latlon_path"
          return
        end if
      end if
    case ("pre_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'pre_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%pre_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%pre_path, 1), &
        lbound(this%pre_path, 1), -1
        if (.not. (this%pre_path(idx) == repeat(achar(0), len(this%pre_path)))) then
          filled(1) = idx - lbound(this%pre_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%pre_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%pre_path(lb_1:ub_1) == repeat(achar(0), len(this%pre_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: pre_path"
          return
        end if
      end if
    case ("pet_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'pet_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%pet_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%pet_path, 1), &
        lbound(this%pet_path, 1), -1
        if (.not. (this%pet_path(idx) == repeat(achar(0), len(this%pet_path)))) then
          filled(1) = idx - lbound(this%pet_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%pet_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%pet_path(lb_1:ub_1) == repeat(achar(0), len(this%pet_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: pet_path"
          return
        end if
      end if
    case ("temp_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'temp_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%temp_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%temp_path, 1), &
        lbound(this%temp_path, 1), -1
        if (.not. (this%temp_path(idx) == repeat(achar(0), len(this%temp_path)))) then
          filled(1) = idx - lbound(this%temp_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%temp_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%temp_path(lb_1:ub_1) == repeat(achar(0), len(this%temp_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: temp_path"
          return
        end if
      end if
    case ("tann_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'tann_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%tann_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%tann_path, 1), &
        lbound(this%tann_path, 1), -1
        if (.not. (this%tann_path(idx) == repeat(achar(0), len(this%tann_path)))) then
          filled(1) = idx - lbound(this%tann_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%tann_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%tann_path(lb_1:ub_1) == repeat(achar(0), len(this%tann_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: tann_path"
          return
        end if
      end if
    case ("tmin_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'tmin_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%tmin_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%tmin_path, 1), &
        lbound(this%tmin_path, 1), -1
        if (.not. (this%tmin_path(idx) == repeat(achar(0), len(this%tmin_path)))) then
          filled(1) = idx - lbound(this%tmin_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%tmin_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%tmin_path(lb_1:ub_1) == repeat(achar(0), len(this%tmin_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: tmin_path"
          return
        end if
      end if
    case ("tmax_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'tmax_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%tmax_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%tmax_path, 1), &
        lbound(this%tmax_path, 1), -1
        if (.not. (this%tmax_path(idx) == repeat(achar(0), len(this%tmax_path)))) then
          filled(1) = idx - lbound(this%tmax_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%tmax_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%tmax_path(lb_1:ub_1) == repeat(achar(0), len(this%tmax_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: tmax_path"
          return
        end if
      end if
    case ("ssrd_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'ssrd_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%ssrd_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%ssrd_path, 1), &
        lbound(this%ssrd_path, 1), -1
        if (.not. (this%ssrd_path(idx) == repeat(achar(0), len(this%ssrd_path)))) then
          filled(1) = idx - lbound(this%ssrd_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%ssrd_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%ssrd_path(lb_1:ub_1) == repeat(achar(0), len(this%ssrd_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: ssrd_path"
          return
        end if
      end if
    case ("strd_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'strd_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%strd_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%strd_path, 1), &
        lbound(this%strd_path, 1), -1
        if (.not. (this%strd_path(idx) == repeat(achar(0), len(this%strd_path)))) then
          filled(1) = idx - lbound(this%strd_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%strd_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%strd_path(lb_1:ub_1) == repeat(achar(0), len(this%strd_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: strd_path"
          return
        end if
      end if
    case ("netrad_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'netrad_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%netrad_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%netrad_path, 1), &
        lbound(this%netrad_path, 1), -1
        if (.not. (this%netrad_path(idx) == repeat(achar(0), len(this%netrad_path)))) then
          filled(1) = idx - lbound(this%netrad_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%netrad_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%netrad_path(lb_1:ub_1) == repeat(achar(0), len(this%netrad_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: netrad_path"
          return
        end if
      end if
    case ("eabs_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'eabs_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%eabs_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%eabs_path, 1), &
        lbound(this%eabs_path, 1), -1
        if (.not. (this%eabs_path(idx) == repeat(achar(0), len(this%eabs_path)))) then
          filled(1) = idx - lbound(this%eabs_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%eabs_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%eabs_path(lb_1:ub_1) == repeat(achar(0), len(this%eabs_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: eabs_path"
          return
        end if
      end if
    case ("wind_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'wind_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%wind_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%wind_path, 1), &
        lbound(this%wind_path, 1), -1
        if (.not. (this%wind_path(idx) == repeat(achar(0), len(this%wind_path)))) then
          filled(1) = idx - lbound(this%wind_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%wind_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%wind_path(lb_1:ub_1) == repeat(achar(0), len(this%wind_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: wind_path"
          return
        end if
      end if
    case ("meteo_mask_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'meteo_mask_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%meteo_mask_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%meteo_mask_path, 1), &
        lbound(this%meteo_mask_path, 1), -1
        if (.not. (this%meteo_mask_path(idx) == repeat(achar(0), len(this%meteo_mask_path)))) then
          filled(1) = idx - lbound(this%meteo_mask_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%meteo_mask_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%meteo_mask_path(lb_1:ub_1) == repeat(achar(0), len(this%meteo_mask_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: meteo_mask_path"
          return
        end if
      end if
    case ("runoff_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'runoff_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%runoff_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%runoff_path, 1), &
        lbound(this%runoff_path, 1), -1
        if (.not. (this%runoff_path(idx) == repeat(achar(0), len(this%runoff_path)))) then
          filled(1) = idx - lbound(this%runoff_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%runoff_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%runoff_path(lb_1:ub_1) == repeat(achar(0), len(this%runoff_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: runoff_path"
          return
        end if
      end if
    case ("runoff_sealed_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'runoff_sealed_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%runoff_sealed_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%runoff_sealed_path, 1), &
        lbound(this%runoff_sealed_path, 1), -1
        if (.not. (this%runoff_sealed_path(idx) == repeat(achar(0), len(this%runoff_sealed_path)))) then
          filled(1) = idx - lbound(this%runoff_sealed_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%runoff_sealed_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%runoff_sealed_path(lb_1:ub_1) == repeat(achar(0), len(this%runoff_sealed_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: runoff_sealed_path"
          return
        end if
      end if
    case ("interflow_fast_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'interflow_fast_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%interflow_fast_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%interflow_fast_path, 1), &
        lbound(this%interflow_fast_path, 1), -1
        if (.not. (this%interflow_fast_path(idx) == repeat(achar(0), len(this%interflow_fast_path)))) then
          filled(1) = idx - lbound(this%interflow_fast_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%interflow_fast_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%interflow_fast_path(lb_1:ub_1) == repeat(achar(0), len(this%interflow_fast_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: interflow_fast_path"
          return
        end if
      end if
    case ("interflow_slow_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'interflow_slow_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%interflow_slow_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%interflow_slow_path, 1), &
        lbound(this%interflow_slow_path, 1), -1
        if (.not. (this%interflow_slow_path(idx) == repeat(achar(0), len(this%interflow_slow_path)))) then
          filled(1) = idx - lbound(this%interflow_slow_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%interflow_slow_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%interflow_slow_path(lb_1:ub_1) == repeat(achar(0), len(this%interflow_slow_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: interflow_slow_path"
          return
        end if
      end if
    case ("baseflow_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'baseflow_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%baseflow_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%baseflow_path, 1), &
        lbound(this%baseflow_path, 1), -1
        if (.not. (this%baseflow_path(idx) == repeat(achar(0), len(this%baseflow_path)))) then
          filled(1) = idx - lbound(this%baseflow_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%baseflow_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%baseflow_path(lb_1:ub_1) == repeat(achar(0), len(this%baseflow_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: baseflow_path"
          return
        end if
      end if
    case ("hydro_mask_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'hydro_mask_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%hydro_mask_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%hydro_mask_path, 1), &
        lbound(this%hydro_mask_path, 1), -1
        if (.not. (this%hydro_mask_path(idx) == repeat(achar(0), len(this%hydro_mask_path)))) then
          filled(1) = idx - lbound(this%hydro_mask_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%hydro_mask_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%hydro_mask_path(lb_1:ub_1) == repeat(achar(0), len(this%hydro_mask_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: hydro_mask_path"
          return
        end if
      end if
    case ("dem_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'dem_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%dem_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%dem_path, 1), &
        lbound(this%dem_path, 1), -1
        if (.not. (this%dem_path(idx) == repeat(achar(0), len(this%dem_path)))) then
          filled(1) = idx - lbound(this%dem_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%dem_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%dem_path(lb_1:ub_1) == repeat(achar(0), len(this%dem_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: dem_path"
          return
        end if
      end if
    case ("slope_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'slope_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%slope_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%slope_path, 1), &
        lbound(this%slope_path, 1), -1
        if (.not. (this%slope_path(idx) == repeat(achar(0), len(this%slope_path)))) then
          filled(1) = idx - lbound(this%slope_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%slope_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%slope_path(lb_1:ub_1) == repeat(achar(0), len(this%slope_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: slope_path"
          return
        end if
      end if
    case ("aspect_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'aspect_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%aspect_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%aspect_path, 1), &
        lbound(this%aspect_path, 1), -1
        if (.not. (this%aspect_path(idx) == repeat(achar(0), len(this%aspect_path)))) then
          filled(1) = idx - lbound(this%aspect_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%aspect_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%aspect_path(lb_1:ub_1) == repeat(achar(0), len(this%aspect_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: aspect_path"
          return
        end if
      end if
    case ("geo_class_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'geo_class_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%geo_class_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%geo_class_path, 1), &
        lbound(this%geo_class_path, 1), -1
        if (.not. (this%geo_class_path(idx) == repeat(achar(0), len(this%geo_class_path)))) then
          filled(1) = idx - lbound(this%geo_class_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%geo_class_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%geo_class_path(lb_1:ub_1) == repeat(achar(0), len(this%geo_class_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: geo_class_path"
          return
        end if
      end if
    case ("soil_class_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'soil_class_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%soil_class_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%soil_class_path, 1), &
        lbound(this%soil_class_path, 1), -1
        if (.not. (this%soil_class_path(idx) == repeat(achar(0), len(this%soil_class_path)))) then
          filled(1) = idx - lbound(this%soil_class_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%soil_class_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%soil_class_path(lb_1:ub_1) == repeat(achar(0), len(this%soil_class_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: soil_class_path"
          return
        end if
      end if
    case ("soil_horizon_class_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'soil_horizon_class_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%soil_horizon_class_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%soil_horizon_class_path, 1), &
        lbound(this%soil_horizon_class_path, 1), -1
        if (.not. (this%soil_horizon_class_path(idx) == repeat(achar(0), len(this%soil_horizon_class_path)))) then
          filled(1) = idx - lbound(this%soil_horizon_class_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%soil_horizon_class_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%soil_horizon_class_path(lb_1:ub_1) == repeat(achar(0), len(this%soil_horizon_class_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: soil_horizon_class_path"
          return
        end if
      end if
    case ("lai_class_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'lai_class_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%lai_class_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%lai_class_path, 1), &
        lbound(this%lai_class_path, 1), -1
        if (.not. (this%lai_class_path(idx) == repeat(achar(0), len(this%lai_class_path)))) then
          filled(1) = idx - lbound(this%lai_class_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%lai_class_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%lai_class_path(lb_1:ub_1) == repeat(achar(0), len(this%lai_class_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: lai_class_path"
          return
        end if
      end if
    case ("river_width_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'river_width_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%river_width_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%river_width_path, 1), &
        lbound(this%river_width_path, 1), -1
        if (.not. (this%river_width_path(idx) == repeat(achar(0), len(this%river_width_path)))) then
          filled(1) = idx - lbound(this%river_width_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%river_width_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%river_width_path(lb_1:ub_1) == repeat(achar(0), len(this%river_width_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: river_width_path"
          return
        end if
      end if
    case ("morph_mask_path")
      if (size(filled) /= 1) then
        status = NML_ERR_INVALID_INDEX
        if (present(errmsg)) errmsg = "shape rank mismatch for 'morph_mask_path'"
        return
      end if
      do dim = 1, 1
        filled(dim) = size(this%morph_mask_path, dim)
      end do
      filled(1) = 0
      do idx = ubound(this%morph_mask_path, 1), &
        lbound(this%morph_mask_path, 1), -1
        if (.not. (this%morph_mask_path(idx) == repeat(achar(0), len(this%morph_mask_path)))) then
          filled(1) = idx - lbound(this%morph_mask_path, 1) + 1
          exit
        end if
      end do
      if (minval(filled) > 0) then
        lb_1 = lbound(this%morph_mask_path, 1)
        ub_1 = lb_1 + filled(1) - 1
        if (any(this%morph_mask_path(lb_1:ub_1) == repeat(achar(0), len(this%morph_mask_path)))) then
          status = NML_ERR_PARTLY_SET
          if (present(errmsg)) errmsg = "array partly set: morph_mask_path"
          return
        end if
      end if
    case default
      status = NML_ERR_INVALID_NAME
      if (present(errmsg)) errmsg = "field is not a flexible array: " // trim(name)
    end select
  end function nml_config_input_filled_shape

  !> \brief Validate required values and constraints
  integer function nml_config_input_is_valid(this, errmsg) result(status)
    class(nml_config_input_t), intent(in) :: this
    character(len=*), intent(out), optional :: errmsg
    integer :: istat
    integer, allocatable :: filled(:)

    status = NML_OK
    if (present(errmsg)) errmsg = ""

    ! flexible arrays
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("latlon_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: latlon_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("pre_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: pre_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("pet_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: pet_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("temp_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: temp_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("tann_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: tann_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("tmin_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: tmin_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("tmax_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: tmax_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("ssrd_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: ssrd_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("strd_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: strd_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("netrad_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: netrad_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("eabs_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: eabs_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("wind_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: wind_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("meteo_mask_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: meteo_mask_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("runoff_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: runoff_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("runoff_sealed_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: runoff_sealed_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("interflow_fast_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: interflow_fast_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("interflow_slow_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: interflow_slow_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("baseflow_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: baseflow_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("hydro_mask_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: hydro_mask_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("dem_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: dem_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("slope_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: slope_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("aspect_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: aspect_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("geo_class_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: geo_class_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("soil_class_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: soil_class_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("soil_horizon_class_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: soil_horizon_class_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("lai_class_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: lai_class_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("river_width_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: river_width_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    if (allocated(filled)) deallocate(filled)
    allocate(filled(1))
    istat = this%filled_shape("morph_mask_path", filled, errmsg=errmsg)
    if (istat == NML_ERR_PARTLY_SET) then
      status = istat
      if (present(errmsg)) then
        if (len_trim(errmsg) == 0) errmsg = "array partly set: morph_mask_path"
      end if
      return
    end if
    if (istat /= NML_OK) then
      status = istat
      return
    end if
    ! enum constraints
    if (.not. all(time_stamp_location_in_enum(this%time_stamp_location, allow_missing=.true.))) then
      status = NML_ERR_ENUM
      if (present(errmsg)) errmsg = "enum constraint failed: time_stamp_location"
      return
    end if
    ! bounds constraints
    if (.not. all(chunking_in_bounds(this%chunking, allow_missing=.true.))) then
      status = NML_ERR_BOUNDS
      if (present(errmsg)) errmsg = "bounds constraint failed: chunking"
      return
    end if
  end function nml_config_input_is_valid

end module nml_config_input
