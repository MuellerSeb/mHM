# PET - Case -1 {#petminus1}

[TOC]

Parameters for PET (case -1 - LAI correction).

**Namelist**: `petminus1`

## Fields

| Name | Type | Required | Info |
| --- | --- | --- | --- |
| [PET_a_forest](#pet_a_forest) | real array | yes | Potential evapotranspiration forest |
| [PET_a_impervious](#pet_a_impervious) | real array | yes | Potential evapotranspiration impervious |
| [PET_a_pervious](#pet_a_pervious) | real array | yes | Potential evapotranspiration pervious |
| [PET_b](#pet_b) | real array | yes | Potential evapotranspiration b |
| [PET_c](#pet_c) | real array | yes | Potential evapotranspiration c |

## Field details

### PET_a_forest

Potential evapotranspiration forest `PET_a_forest`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.3, 1.3, 0.3, 1.0, 1.0]`

### PET_a_impervious

Potential evapotranspiration impervious `PET_a_impervious`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.3, 1.3, 0.8, 1.0, 1.0]`

### PET_a_pervious

Potential evapotranspiration pervious `PET_a_pervious`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.3, 1.3, 1.3, 1.0, 1.0]`

### PET_b

Potential evapotranspiration b `PET_b`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.0, 1.5, 1.5, 1.0, 1.0]`

### PET_c

Potential evapotranspiration c `PET_c`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[-2.0, 0.0, -0.7, 1.0, 1.0]`

## Example

```fortran
&petminus1
  PET_a_forest(:) = 0.3, 1.3, 0.3, 1.0, 1.0
  PET_a_impervious(:) = 0.3, 1.3, 0.8, 1.0, 1.0
  PET_a_pervious(:) = 0.3, 1.3, 1.3, 1.0, 1.0
  PET_b(:) = 0.0, 1.5, 1.5, 1.0, 1.0
  PET_c(:) = -2.0, 0.0, -0.7, 1.0, 1.0
/
```

