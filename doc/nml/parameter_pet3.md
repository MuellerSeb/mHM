# PET - Case 3 {#pet3}

[TOC]

Parameters for PET (case 3 - Penman-Monteith).

**Namelist**: `pet3`

## Fields

| Name | Type | Required | Info |
| --- | --- | --- | --- |
| [canopyheigth_forest](#canopyheigth_forest) | real array | yes | Canopy height forest |
| [canopyheigth_impervious](#canopyheigth_impervious) | real array | yes | Canopy height impervious |
| [canopyheigth_pervious](#canopyheigth_pervious) | real array | yes | Canopy height pervious |
| [displacementheight_coeff](#displacementheight_coeff) | real array | yes | Displacement height coefficient |
| [roughnesslength_momentum_coeff](#roughnesslength_momentum_coeff) | real array | yes | Roughness length momentum coefficient |
| [roughnesslength_heat_coeff](#roughnesslength_heat_coeff) | real array | yes | Roughness length heat coefficient |
| [stomatal_resistance](#stomatal_resistance) | real array | yes | Stomatal resistance |

## Field details

### canopyheigth_forest

Canopy height forest `canopyheigth_forest`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[15.0, 40.0, 15.0, 1.0, 1.0]`

### canopyheigth_impervious

Canopy height impervious `canopyheigth_impervious`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.01, 0.5, 0.02, 1.0, 1.0]`

### canopyheigth_pervious

Canopy height pervious `canopyheigth_pervious`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.1, 5.0, 0.11, 1.0, 1.0]`

### displacementheight_coeff

Displacement height coefficient `displacementheight_coeff`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.5, 0.85, 0.64, 1.0, 1.0]`

### roughnesslength_momentum_coeff

Roughness length momentum coefficient `roughnesslength_momentum_coeff`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.09, 0.16, 0.095, 1.0, 1.0]`

### roughnesslength_heat_coeff

Roughness length heat coefficient `roughnesslength_heat_coeff`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.07, 0.13, 0.075, 1.0, 1.0]`

### stomatal_resistance

Stomatal resistance `stomatal_resistance`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[10.0, 200.0, 56.0, 1.0, 1.0]`

## Example

```fortran
&pet3
  canopyheigth_forest(:) = 15.0, 40.0, 15.0, 1.0, 1.0
  canopyheigth_impervious(:) = 0.01, 0.5, 0.02, 1.0, 1.0
  canopyheigth_pervious(:) = 0.1, 5.0, 0.11, 1.0, 1.0
  displacementheight_coeff(:) = 0.5, 0.85, 0.64, 1.0, 1.0
  roughnesslength_momentum_coeff(:) = 0.09, 0.16, 0.095, 1.0, 1.0
  roughnesslength_heat_coeff(:) = 0.07, 0.13, 0.075, 1.0, 1.0
  stomatal_resistance(:) = 10.0, 200.0, 56.0, 1.0, 1.0
/
```

