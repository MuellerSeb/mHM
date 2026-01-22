# Coupling configuration {#config_coupling}

[TOC]

Coupling grid and variable settings for meteorological, hydrological, and morphology components.
Arrays are indexed by domain (dimension 1).
Grid parameters define resolutions and coordinate systems for coupled runs.
Coupled flags indicate whether inputs are provided by an external model.

**Namelist**: `config_coupling`

## Fields

| Name | Type | Required | Info |
| --- | --- | --- | --- |
| `meteo_grid_nx` | integer array | no | Meteo grid size in x-direction |
| `meteo_grid_ny` | integer array | no | Meteo grid size in y-direction |
| `meteo_grid_xll` | real array | no | Meteo grid x origin |
| `meteo_grid_yll` | real array | no | Meteo grid y origin |
| `meteo_grid_cellsize` | real array | no | Meteo grid cell size |
| `meteo_grid_ydir` | integer array | no | Meteo grid y direction |
| `meteo_grid_coordsys` | integer array | no | Meteo grid coordinate system |
| `hydro_grid_nx` | integer array | no | Hydro grid size in x-direction |
| `hydro_grid_ny` | integer array | no | Hydro grid size in y-direction |
| `hydro_grid_xll` | real array | no | Hydro grid x origin |
| `hydro_grid_yll` | real array | no | Hydro grid y origin |
| `hydro_grid_cellsize` | real array | no | Hydro grid cell size |
| `hydro_grid_ydir` | integer array | no | Hydro grid y direction |
| `hydro_grid_coordsys` | integer array | no | Hydro grid coordinate system |
| `morph_grid_nx` | integer array | no | Morph grid size in x-direction |
| `morph_grid_ny` | integer array | no | Morph grid size in y-direction |
| `morph_grid_xll` | real array | no | Morph grid x origin |
| `morph_grid_yll` | real array | no | Morph grid y origin |
| `morph_grid_cellsize` | real array | no | Morph grid cell size |
| `morph_grid_ydir` | integer array | no | Morph grid y direction |
| `morph_grid_coordsys` | integer array | no | Morph grid coordinate system |
| `pre_coupled` | logical array | no | Precipitation coupled |
| `pet_coupled` | logical array | no | Potential evapotranspiration coupled |
| `temp_coupled` | logical array | no | Air temperature coupled |
| `tann_coupled` | logical array | no | Air temperature annual mean coupled |
| `tmin_coupled` | logical array | no | Air temperature daily minimum coupled |
| `tmax_coupled` | logical array | no | Air temperature daily maximum coupled |
| `ssrd_coupled` | logical array | no | Surface shortwave radiation coupled |
| `strd_coupled` | logical array | no | Surface thermal radiation coupled |
| `netrad_coupled` | logical array | no | Net radiation coupled |
| `eabs_coupled` | logical array | no | Vapor pressure coupled |
| `wind_coupled` | logical array | no | Wind speed coupled |
| `runoff_coupled` | logical array | no | Runoff coupled |
| `runoff_sealed_coupled` | logical array | no | Sealed runoff coupled |
| `interflow_fast_coupled` | logical array | no | Fast interflow coupled |
| `interflow_slow_coupled` | logical array | no | Slow interflow coupled |
| `baseflow_coupled` | logical array | no | Baseflow coupled |
| `dem_coupled` | logical array | no | DEM coupled |
| `slope_coupled` | logical array | no | Slope coupled |
| `aspect_coupled` | logical array | no | Aspect coupled |
| `geo_class_coupled` | logical array | no | Geology class coupled |
| `soil_class_coupled` | logical array | no | Soil class coupled |
| `lai_class_coupled` | logical array | no | LAI class coupled |
| `river_width_coupled` | logical array | no | River width coupled |
| `meteo_mask_coupled` | logical array | no | Meteorological mask coupled |
| `hydro_mask_coupled` | logical array | no | Hydrological mask coupled |
| `morph_mask_coupled` | logical array | no | Morphology mask coupled |
| `hydro_latlon_coupled` | logical array | no | Hydrological latlon coupled |
| `morph_latlon_coupled` | logical array | no | Morphological latlon coupled |
| `route_latlon_coupled` | logical array | no | Routing latlon coupled |

## Field details

### `meteo_grid_nx` - Meteo grid size in x-direction

Summary:
- Type: `integer(i4), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `meteo_grid_ny` - Meteo grid size in y-direction

Summary:
- Type: `integer(i4), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `meteo_grid_xll` - Meteo grid x origin

Summary:
- Type: `real(dp), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `meteo_grid_yll` - Meteo grid y origin

Summary:
- Type: `real(dp), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `meteo_grid_cellsize` - Meteo grid cell size

Summary:
- Type: `real(dp), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `meteo_grid_ydir` - Meteo grid y direction

Direction of the y-axis for the meteorological grid.
- 0 : top-down y-axis
- 1 : bottom-up y-axis

Summary:
- Type: `integer(i4), dimension(max_domains)`
- Required: no
- Default: `0`
- Allowed values: `0`, `1`

### `meteo_grid_coordsys` - Meteo grid coordinate system

Coordinate system for the meteorological grid.
- 0 : cartesian
- 1 : latlon

Summary:
- Type: `integer(i4), dimension(max_domains)`
- Required: no
- Default: `0`
- Allowed values: `0`, `1`

### `hydro_grid_nx` - Hydro grid size in x-direction

Summary:
- Type: `integer(i4), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `hydro_grid_ny` - Hydro grid size in y-direction

Summary:
- Type: `integer(i4), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `hydro_grid_xll` - Hydro grid x origin

Summary:
- Type: `real(dp), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `hydro_grid_yll` - Hydro grid y origin

Summary:
- Type: `real(dp), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `hydro_grid_cellsize` - Hydro grid cell size

Summary:
- Type: `real(dp), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `hydro_grid_ydir` - Hydro grid y direction

Direction of the y-axis for the hydrological grid.
- 0 : top-down y-axis
- 1 : bottom-up y-axis

Summary:
- Type: `integer(i4), dimension(max_domains)`
- Required: no
- Default: `0`
- Allowed values: `0`, `1`

### `hydro_grid_coordsys` - Hydro grid coordinate system

Coordinate system for the hydrological grid.
- 0 : cartesian
- 1 : latlon

Summary:
- Type: `integer(i4), dimension(max_domains)`
- Required: no
- Default: `0`
- Allowed values: `0`, `1`

### `morph_grid_nx` - Morph grid size in x-direction

Summary:
- Type: `integer(i4), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `morph_grid_ny` - Morph grid size in y-direction

Summary:
- Type: `integer(i4), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `morph_grid_xll` - Morph grid x origin

Summary:
- Type: `real(dp), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `morph_grid_yll` - Morph grid y origin

Summary:
- Type: `real(dp), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `morph_grid_cellsize` - Morph grid cell size

Summary:
- Type: `real(dp), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `morph_grid_ydir` - Morph grid y direction

Direction of the y-axis for the morphology grid.
- 0 : top-down y-axis
- 1 : bottom-up y-axis

Summary:
- Type: `integer(i4), dimension(max_domains)`
- Required: no
- Default: `0`
- Allowed values: `0`, `1`

### `morph_grid_coordsys` - Morph grid coordinate system

Coordinate system for the morphology grid.
- 0 : cartesian
- 1 : latlon

Summary:
- Type: `integer(i4), dimension(max_domains)`
- Required: no
- Default: `0`
- Allowed values: `0`, `1`

### `pre_coupled` - Precipitation coupled

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### `pet_coupled` - Potential evapotranspiration coupled

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### `temp_coupled` - Air temperature coupled

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### `tann_coupled` - Air temperature annual mean coupled

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### `tmin_coupled` - Air temperature daily minimum coupled

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### `tmax_coupled` - Air temperature daily maximum coupled

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### `ssrd_coupled` - Surface shortwave radiation coupled

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### `strd_coupled` - Surface thermal radiation coupled

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### `netrad_coupled` - Net radiation coupled

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### `eabs_coupled` - Vapor pressure coupled

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### `wind_coupled` - Wind speed coupled

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### `runoff_coupled` - Runoff coupled

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### `runoff_sealed_coupled` - Sealed runoff coupled

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### `interflow_fast_coupled` - Fast interflow coupled

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### `interflow_slow_coupled` - Slow interflow coupled

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### `baseflow_coupled` - Baseflow coupled

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### `dem_coupled` - DEM coupled

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### `slope_coupled` - Slope coupled

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### `aspect_coupled` - Aspect coupled

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### `geo_class_coupled` - Geology class coupled

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### `soil_class_coupled` - Soil class coupled

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### `lai_class_coupled` - LAI class coupled

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### `river_width_coupled` - River width coupled

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### `meteo_mask_coupled` - Meteorological mask coupled

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### `hydro_mask_coupled` - Hydrological mask coupled

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### `morph_mask_coupled` - Morphology mask coupled

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### `hydro_latlon_coupled` - Hydrological latlon coupled

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### `morph_latlon_coupled` - Morphological latlon coupled

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### `route_latlon_coupled` - Routing latlon coupled

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

## Example

```fortran
&config_coupling
  meteo_grid_nx(:) = 0
  meteo_grid_ny(:) = 0
  meteo_grid_xll(:) = 0.0
  meteo_grid_yll(:) = 0.0
  meteo_grid_cellsize(:) = 0.0
  meteo_grid_ydir(:) = 0
  meteo_grid_coordsys(:) = 0
  hydro_grid_nx(:) = 0
  hydro_grid_ny(:) = 0
  hydro_grid_xll(:) = 0.0
  hydro_grid_yll(:) = 0.0
  hydro_grid_cellsize(:) = 0.0
  hydro_grid_ydir(:) = 0
  hydro_grid_coordsys(:) = 0
  morph_grid_nx(:) = 0
  morph_grid_ny(:) = 0
  morph_grid_xll(:) = 0.0
  morph_grid_yll(:) = 0.0
  morph_grid_cellsize(:) = 0.0
  morph_grid_ydir(:) = 0
  morph_grid_coordsys(:) = 0
  pre_coupled(:) = .false.
  pet_coupled(:) = .false.
  temp_coupled(:) = .false.
  tann_coupled(:) = .false.
  tmin_coupled(:) = .false.
  tmax_coupled(:) = .false.
  ssrd_coupled(:) = .false.
  strd_coupled(:) = .false.
  netrad_coupled(:) = .false.
  eabs_coupled(:) = .false.
  wind_coupled(:) = .false.
  runoff_coupled(:) = .false.
  runoff_sealed_coupled(:) = .false.
  interflow_fast_coupled(:) = .false.
  interflow_slow_coupled(:) = .false.
  baseflow_coupled(:) = .false.
  dem_coupled(:) = .false.
  slope_coupled(:) = .false.
  aspect_coupled(:) = .false.
  geo_class_coupled(:) = .false.
  soil_class_coupled(:) = .false.
  lai_class_coupled(:) = .false.
  river_width_coupled(:) = .false.
  meteo_mask_coupled(:) = .false.
  hydro_mask_coupled(:) = .false.
  morph_mask_coupled(:) = .false.
  hydro_latlon_coupled(:) = .false.
  morph_latlon_coupled(:) = .false.
  route_latlon_coupled(:) = .false.
/
```

