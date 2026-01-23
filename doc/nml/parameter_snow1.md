# Snow - Case 1 {#snow1}

[TOC]

Parameters for Snow module.

**Namelist**: `snow1`

## Fields

| Name | Type | Required | Info |
| --- | --- | --- | --- |
| [snowTreshholdTemperature](#snowtreshholdtemperature) | real array | yes | Threshold for rain/snow partitioning [degC]. |
| [degreeDayFactor_forest](#degreedayfactor_forest) | real array | yes | Degree day factors to determine melting flux [m degC-1]. |
| [degreeDayFactor_impervious](#degreedayfactor_impervious) | real array | yes | Degree day factors to determine melting flux [m degC-1]. |
| [degreeDayFactor_pervious](#degreedayfactor_pervious) | real array | yes | Degree day factors to determine melting flux [m degC-1]. |
| [increaseDegreeDayFactorByPrecip](#increasedegreedayfactorbyprecip) | real array | yes | Increase of degree day factor if there is precipitation [degC-1]. |
| [maxDegreeDayFactor_forest](#maxdegreedayfactor_forest) | real array | yes | Maximum values for degree day factor [m degC-1]. |
| [maxDegreeDayFactor_impervious](#maxdegreedayfactor_impervious) | real array | yes | Maximum values for degree day factor [m degC-1]. |
| [maxDegreeDayFactor_pervious](#maxdegreedayfactor_pervious) | real array | yes | Maximum values for degree day factor [m degC-1]. |

## Field details

### snowTreshholdTemperature

Threshold for rain/snow partitioning [degC]. `snowTreshholdTemperature`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[-2.0, 2.0, 1.0, 1.0, 1.0]`

### degreeDayFactor_forest

Degree day factors to determine melting flux [m degC-1]. `degreeDayFactor_forest`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.0001, 4.0, 1.5, 1.0, 1.0]`

### degreeDayFactor_impervious

Degree day factors to determine melting flux [m degC-1]. `degreeDayFactor_impervious`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.0, 1.0, 0.5, 1.0, 1.0]`

### degreeDayFactor_pervious

Degree day factors to determine melting flux [m degC-1]. `degreeDayFactor_pervious`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.0, 2.0, 0.5, 1.0, 1.0]`

### increaseDegreeDayFactorByPrecip

Increase of degree day factor if there is precipitation [degC-1]. `increaseDegreeDayFactorByPrecip`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.1, 0.9, 0.5, 1.0, 1.0]`

### maxDegreeDayFactor_forest

Maximum values for degree day factor [m degC-1]. `maxDegreeDayFactor_forest`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.0, 8.0, 3.0, 1.0, 1.0]`

### maxDegreeDayFactor_impervious

Maximum values for degree day factor [m degC-1]. `maxDegreeDayFactor_impervious`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.0, 8.0, 3.5, 1.0, 1.0]`

### maxDegreeDayFactor_pervious

Maximum values for degree day factor [m degC-1]. `maxDegreeDayFactor_pervious`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.0, 8.0, 4.0, 1.0, 1.0]`

## Example

```fortran
&snow1
  snowTreshholdTemperature(:) = -2.0, 2.0, 1.0, 1.0, 1.0
  degreeDayFactor_forest(:) = 0.0001, 4.0, 1.5, 1.0, 1.0
  degreeDayFactor_impervious(:) = 0.0, 1.0, 0.5, 1.0, 1.0
  degreeDayFactor_pervious(:) = 0.0, 2.0, 0.5, 1.0, 1.0
  increaseDegreeDayFactorByPrecip(:) = 0.1, 0.9, 0.5, 1.0, 1.0
  maxDegreeDayFactor_forest(:) = 0.0, 8.0, 3.0, 1.0, 1.0
  maxDegreeDayFactor_impervious(:) = 0.0, 8.0, 3.5, 1.0, 1.0
  maxDegreeDayFactor_pervious(:) = 0.0, 8.0, 4.0, 1.0, 1.0
/
```

