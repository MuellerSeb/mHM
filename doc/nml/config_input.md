# Input configuration {#config_input}

[TOC]

Paths and variable names for input data used by mHM.
Arrays are indexed by domain (dimension 1). Most paths are optional.
Variable name entries define the NetCDF variable names to read.

**Namelist**: `config_input`

## Fields

| Name | Type | Required | Info |
| --- | --- | --- | --- |
| `chunking` | integer array | no | Chunking for input data |
| `time_stamp_location` | integer array | no | NetCDF time-stamp location |
| `latlon_path` | string array | no | Latlon specification file path |
| `morph_latlon` | logical array | no | DEM in latlon coordinates |
| `pre_path` | string array | no | Precipitation input |
| `pet_path` | string array | no | Potential evapotranspiration input |
| `temp_path` | string array | no | Air temperature input |
| `tann_path` | string array | no | Air temperature annual mean input |
| `tmin_path` | string array | no | Air temperature daily minimum input |
| `tmax_path` | string array | no | Air temperature daily maximum input |
| `ssrd_path` | string array | no | Surface shortwave radiation downwards input |
| `strd_path` | string array | no | Surface thermal radiation downwards input |
| `netrad_path` | string array | no | Net radiation input |
| `eabs_path` | string array | no | Vapor pressure input |
| `wind_path` | string array | no | Wind speed input |
| `meteo_mask_path` | string array | no | Meteorological mask file path |
| `runoff_path` | string array | no | Runoff input |
| `runoff_sealed_path` | string array | no | Sealed runoff input |
| `interflow_fast_path` | string array | no | Fast interflow input |
| `interflow_slow_path` | string array | no | Slow interflow input |
| `baseflow_path` | string array | no | Baseflow input |
| `hydro_mask_path` | string array | no | Hydrological mask file path |
| `dem_path` | string array | no | DEM input |
| `slope_path` | string array | no | Slope input |
| `aspect_path` | string array | no | Aspect input |
| `geo_class_path` | string array | no | Geology class input |
| `soil_class_path` | string array | no | Soil class input |
| `soil_horizon_class_path` | string array | no | Soil horizon class input |
| `lai_class_path` | string array | no | LAI class input |
| `river_width_path` | string array | no | River width input |
| `morph_mask_path` | string array | no | Morphology mask file path |
| `pre_var` | string array | no | Precipitation variable name |
| `pet_var` | string array | no | Potential evapotranspiration variable name |
| `temp_var` | string array | no | Air temperature variable name |
| `tann_var` | string array | no | Air temperature annual mean variable name |
| `tmin_var` | string array | no | Air temperature daily minimum variable name |
| `tmax_var` | string array | no | Air temperature daily maximum variable name |
| `ssrd_var` | string array | no | Surface shortwave radiation variable name |
| `strd_var` | string array | no | Surface thermal radiation variable name |
| `netrad_var` | string array | no | Net radiation variable name |
| `eabs_var` | string array | no | Vapor pressure variable name |
| `wind_var` | string array | no | Wind speed variable name |
| `meteo_mask_var` | string array | no | Meteorological mask variable name |
| `runoff_var` | string array | no | Runoff variable name |
| `runoff_sealed_var` | string array | no | Sealed runoff variable name |
| `interflow_fast_var` | string array | no | Fast interflow variable name |
| `interflow_slow_var` | string array | no | Slow interflow variable name |
| `baseflow_var` | string array | no | Baseflow variable name |
| `hydro_mask_var` | string array | no | Hydrological mask variable name |
| `dem_var` | string array | no | DEM variable name |
| `slope_var` | string array | no | Slope variable name |
| `aspect_var` | string array | no | Aspect variable name |
| `geo_class_var` | string array | no | Geology class variable name |
| `soil_class_var` | string array | no | Soil class variable name |
| `lai_class_var` | string array | no | LAI class variable name |
| `river_width_var` | string array | no | River width variable name |
| `morph_mask_var` | string array | no | Morphology mask variable name |
| `hydro_lat_var` | string array | no | Hydrological latitude variable name |
| `hydro_lon_var` | string array | no | Hydrological longitude variable name |
| `morph_lat_var` | string array | no | Morphology latitude variable name |
| `morph_lon_var` | string array | no | Morphology longitude variable name |
| `route_lat_var` | string array | no | Routing latitude variable name |
| `route_lon_var` | string array | no | Routing longitude variable name |

## Field details

### `chunking` - Chunking for input data

Chunking configuration for reading input data (array dimension 1: domain)
- n>0 : after each n time-steps
- 0 : only at beginning of the run
- -1 : daily
- -2 : monthly
- -3 : yearly

Summary:
- Type: `integer(i4), dimension(max_domains)`
- Required: no
- Default: `0`
- Minimum: `>= -3`

### `time_stamp_location` - NetCDF time-stamp location

NetCDF time-stamp location: when no time-bounds are given in the input data.
- 0 : beginning of time step (default)
- 1 : center of time step
- 2 : end of time step

Summary:
- Type: `integer(i4), dimension(max_domains)`
- Required: no
- Default: `0`
- Allowed values: `0`, `1`, `2`

### `latlon_path` - Latlon specification file path

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `morph_latlon` - DEM in latlon coordinates

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### `pre_path` - Precipitation input

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `pet_path` - Potential evapotranspiration input

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `temp_path` - Air temperature input

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `tann_path` - Air temperature annual mean input

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `tmin_path` - Air temperature daily minimum input

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `tmax_path` - Air temperature daily maximum input

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `ssrd_path` - Surface shortwave radiation downwards input

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `strd_path` - Surface thermal radiation downwards input

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `netrad_path` - Net radiation input

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `eabs_path` - Vapor pressure input

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `wind_path` - Wind speed input

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `meteo_mask_path` - Meteorological mask file path

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `runoff_path` - Runoff input

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `runoff_sealed_path` - Sealed runoff input

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `interflow_fast_path` - Fast interflow input

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `interflow_slow_path` - Slow interflow input

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `baseflow_path` - Baseflow input

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `hydro_mask_path` - Hydrological mask file path

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `dem_path` - DEM input

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `slope_path` - Slope input

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `aspect_path` - Aspect input

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `geo_class_path` - Geology class input

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `soil_class_path` - Soil class input

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `soil_horizon_class_path` - Soil horizon class input

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `lai_class_path` - LAI class input

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `river_width_path` - River width input

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `morph_mask_path` - Morphology mask file path

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `pre_var` - Precipitation variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'pre'`

### `pet_var` - Potential evapotranspiration variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'pet'`

### `temp_var` - Air temperature variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'tavg'`

### `tann_var` - Air temperature annual mean variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'tann'`

### `tmin_var` - Air temperature daily minimum variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'tmin'`

### `tmax_var` - Air temperature daily maximum variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'tmax'`

### `ssrd_var` - Surface shortwave radiation variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'ssrd'`

### `strd_var` - Surface thermal radiation variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'strd'`

### `netrad_var` - Net radiation variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'net_rad'`

### `eabs_var` - Vapor pressure variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'eabs'`

### `wind_var` - Wind speed variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'windspeed'`

### `meteo_mask_var` - Meteorological mask variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'mask'`

### `runoff_var` - Runoff variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'runoff'`

### `runoff_sealed_var` - Sealed runoff variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'runoff_sealed'`

### `interflow_fast_var` - Fast interflow variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'interflow_fast'`

### `interflow_slow_var` - Slow interflow variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'interflow_slow'`

### `baseflow_var` - Baseflow variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'baseflow'`

### `hydro_mask_var` - Hydrological mask variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'mask'`

### `dem_var` - DEM variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'dem'`

### `slope_var` - Slope variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'slope'`

### `aspect_var` - Aspect variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'aspect'`

### `geo_class_var` - Geology class variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'geology_class'`

### `soil_class_var` - Soil class variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'soil_class'`

### `lai_class_var` - LAI class variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'LAI_class'`

### `river_width_var` - River width variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'P_bkfl'`

### `morph_mask_var` - Morphology mask variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'mask'`

### `hydro_lat_var` - Hydrological latitude variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'lat'`

### `hydro_lon_var` - Hydrological longitude variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'lon'`

### `morph_lat_var` - Morphology latitude variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'lat_l0'`

### `morph_lon_var` - Morphology longitude variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'lon_l0'`

### `route_lat_var` - Routing latitude variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'lat_l11'`

### `route_lon_var` - Routing longitude variable name

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Required: no
- Default: `'lon_l11'`

## Example

```fortran
&config_input
  chunking(:) = 0
  time_stamp_location(:) = 0
  latlon_path(:) = ''
  morph_latlon(:) = .false.
  pre_path(:) = ''
  pet_path(:) = ''
  temp_path(:) = ''
  tann_path(:) = ''
  tmin_path(:) = ''
  tmax_path(:) = ''
  ssrd_path(:) = ''
  strd_path(:) = ''
  netrad_path(:) = ''
  eabs_path(:) = ''
  wind_path(:) = ''
  meteo_mask_path(:) = ''
  runoff_path(:) = ''
  runoff_sealed_path(:) = ''
  interflow_fast_path(:) = ''
  interflow_slow_path(:) = ''
  baseflow_path(:) = ''
  hydro_mask_path(:) = ''
  dem_path(:) = ''
  slope_path(:) = ''
  aspect_path(:) = ''
  geo_class_path(:) = ''
  soil_class_path(:) = ''
  soil_horizon_class_path(:) = ''
  lai_class_path(:) = ''
  river_width_path(:) = ''
  morph_mask_path(:) = ''
  pre_var(:) = 'pre'
  pet_var(:) = 'pet'
  temp_var(:) = 'tavg'
  tann_var(:) = 'tann'
  tmin_var(:) = 'tmin'
  tmax_var(:) = 'tmax'
  ssrd_var(:) = 'ssrd'
  strd_var(:) = 'strd'
  netrad_var(:) = 'net_rad'
  eabs_var(:) = 'eabs'
  wind_var(:) = 'windspeed'
  meteo_mask_var(:) = 'mask'
  runoff_var(:) = 'runoff'
  runoff_sealed_var(:) = 'runoff_sealed'
  interflow_fast_var(:) = 'interflow_fast'
  interflow_slow_var(:) = 'interflow_slow'
  baseflow_var(:) = 'baseflow'
  hydro_mask_var(:) = 'mask'
  dem_var(:) = 'dem'
  slope_var(:) = 'slope'
  aspect_var(:) = 'aspect'
  geo_class_var(:) = 'geology_class'
  soil_class_var(:) = 'soil_class'
  lai_class_var(:) = 'LAI_class'
  river_width_var(:) = 'P_bkfl'
  morph_mask_var(:) = 'mask'
  hydro_lat_var(:) = 'lat'
  hydro_lon_var(:) = 'lon'
  morph_lat_var(:) = 'lat_l0'
  morph_lon_var(:) = 'lon_l0'
  route_lat_var(:) = 'lat_l11'
  route_lon_var(:) = 'lon_l11'
/
```

