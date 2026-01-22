# MPR configuration {#config_mpr}

[TOC]

Configuration for the multiscale parameter regionalization in mHM.

**Namelist**: `config_mpr`

## Fields

| Name | Type | Required | Info |
| --- | --- | --- | --- |
| `soil_db_mode` | integer array | no | Soil database mode |
| `n_horizons` | integer array | no | Number of soil horizons |
| `tillage_depth` | integer array | no | Tillage depth |
| `soil_depth` | integer array | no | Soil horizon depth |
| `fracSealed_cityArea` | real array | no | Sealed fraction of city area |
| `land_cover_path` | string array | no | Land cover path |
| `lai_time_step` | integer array | no | LAI time step |
| `lai_path` | string array | no | LAI path |
| `soil_lut_path` | string array | no | Soil LUT path |
| `geo_lut_path` | string array | no | Geology LUT path |
| `lai_lut_path` | string array | no | LAI LUT path |

## Field details

### `soil_db_mode` - Soil database mode

Flag to handle multiple soil database types; valid for all domains.
- 0: classical mHM soil database with soil_class.asc and soil_classdefinition.txt.
  Soil horizons follow nSoilHorizons_mHM and soil_Depth for n-1 layers; last layer depth comes from the LUT.
- 1: harmonised horizon-specific soil database with soil_class_horizon_XX.asc and soil_classdefinition_iFlag_soilDB_1.txt.
  Soil depth is provided for all horizons; the last horizon depth is fixed and uniform;
  tillage depth must match a horizon depth.

Summary:
- Type: `integer(i4), dimension(max_domains)`
- Required: no
- Default: `0`
- Allowed values: `0`, `1`

### `n_horizons` - Number of soil horizons

Number of soil horizons to be modeled (nSoilHorizons_mHM).

Summary:
- Type: `integer(i4), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `tillage_depth` - Tillage depth

Soil depth down to which organic matter is possible [mm].

Summary:
- Type: `integer(i4), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `soil_depth` - Soil horizon depth

Bottom depth of soil horizons with respect to the ground surface [mm], positive downwards.
- if soil_db_mode = 0: provide depths for horizons 1..n-1; last horizon depth comes from the LUT.
- if soil_db_mode = 1: provide depths for all horizons 1..n; a soil_class_horizon_XX.asc is required for each.
Tillage depth should match one of the specified horizon depths.

Summary:
- Type: `integer(i4), dimension(max_layers, max_domains)`
- Flexible tail dims: 2
- Required: no

### `fracSealed_cityArea` - Sealed fraction of city area

Fraction of area within city assumed to be fully sealed [0.0-1.0].

Summary:
- Type: `real(dp), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `land_cover_path` - Land cover path

Land cover data path (periods derived from time bounds or start time stamp assumed).

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `lai_time_step` - LAI time step

Time step for LAI input data [days]:
- if = 1: annual cycle monthly gridded LAI values;
- if = 0: no LAI input data used, static LAI from LUT;
- if = -1 : daily gridded LAI input data;
- if = -2 : monthly gridded LAI input data;
- if = -3 : yearly gridded LAI input data;

Summary:
- Type: `integer(i4), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no
- Allowed values: `-3`, `-2`, `-1`, `0`, `1`

### `lai_path` - LAI path

LAI data path (if `lai_time_step` < 0 or = 1).

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `soil_lut_path` - Soil LUT path

Soil look-up table path.

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `geo_lut_path` - Geology LUT path

Geology look-up table path.

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `lai_lut_path` - LAI LUT path

LAI look-up table path.

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

## Example

```fortran
&config_mpr
  soil_db_mode(:) = 0
  n_horizons(:) = 0
  tillage_depth(:) = 0
  soil_depth(:, :) = 0
  fracSealed_cityArea(:) = 0.0
  land_cover_path(:) = ''
  lai_time_step(:) = -3
  lai_path(:) = ''
  soil_lut_path(:) = ''
  geo_lut_path(:) = ''
  lai_lut_path(:) = ''
/
```

