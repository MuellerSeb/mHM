# Routing - Case 1 {#routing1}

[TOC]

Parameters for routing (case 1 - Muskingum).

**Namelist**: `routing1`

## Fields

| Name | Type | Required | Info |
| --- | --- | --- | --- |
| [muskingumTravelTime_constant](#muskingumtraveltime_constant) | real array | yes | Muskingum travel time constant |
| [muskingumTravelTime_riverLength](#muskingumtraveltime_riverlength) | real array | yes | Muskingum travel time river length |
| [muskingumTravelTime_riverSlope](#muskingumtraveltime_riverslope) | real array | yes | Muskingum travel time river slope |
| [muskingumTravelTime_impervious](#muskingumtraveltime_impervious) | real array | yes | Muskingum travel time impervious |
| [muskingumAttenuation_riverSlope](#muskingumattenuation_riverslope) | real array | yes | Muskingum attenuation river slope |

## Field details

### muskingumTravelTime_constant

Muskingum travel time constant `muskingumTravelTime_constant`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.31, 0.35, 0.325, 1.0, 1.0]`

### muskingumTravelTime_riverLength

Muskingum travel time river length `muskingumTravelTime_riverLength`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.07, 0.08, 0.075, 1.0, 1.0]`

### muskingumTravelTime_riverSlope

Muskingum travel time river slope `muskingumTravelTime_riverSlope`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[1.95, 2.1, 2.0, 1.0, 1.0]`

### muskingumTravelTime_impervious

Muskingum travel time impervious `muskingumTravelTime_impervious`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.09, 0.11, 0.1, 1.0, 1.0]`

### muskingumAttenuation_riverSlope

Muskingum attenuation river slope `muskingumAttenuation_riverSlope`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.01, 0.5, 0.3, 1.0, 1.0]`

## Example

```fortran
&routing1
  muskingumTravelTime_constant(:) = 0.31, 0.35, 0.325, 1.0, 1.0
  muskingumTravelTime_riverLength(:) = 0.07, 0.08, 0.075, 1.0, 1.0
  muskingumTravelTime_riverSlope(:) = 1.95, 2.1, 2.0, 1.0, 1.0
  muskingumTravelTime_impervious(:) = 0.09, 0.11, 0.1, 1.0, 1.0
  muskingumAttenuation_riverSlope(:) = 0.01, 0.5, 0.3, 1.0, 1.0
/
```

