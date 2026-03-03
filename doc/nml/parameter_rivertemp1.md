# River Temperature - Case 1 {#rivertemp1}

[TOC]

Parameters for River Temperature case 1.

**Namelist**: `rivertemp1`

## Fields

| Name | Type | Required | Info |
| --- | --- | --- | --- |
| [albedo_water](#albedo_water) | real array | yes | Albedo of open water [-] |
| [pt_a_water](#pt_a_water) | real array | yes | Priestley-Taylor coefficient for open water [-] |
| [emissivity_water](#emissivity_water) | real array | yes | Emissivity of open water [-] |
| [turb_heat_ex_coeff](#turb_heat_ex_coeff) | real array | yes | Turbulent heat exchange coefficient for open water [W m-2 K-1] |

## Field details

### albedo_water

Albedo of open water [-] `albedo_water`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.03, 0.2, 0.15, 0.0, 1.0]`

### pt_a_water

Priestley-Taylor coefficient for open water [-] `pt_a_water`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[1.2, 2.0, 1.26, 0.0, 1.0]`

### emissivity_water

Emissivity of open water [-] `emissivity_water`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.95, 0.99, 0.96, 0.0, 1.0]`

### turb_heat_ex_coeff

Turbulent heat exchange coefficient for open water [W m-2 K-1] `turb_heat_ex_coeff`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[10.0, 50.0, 20.0, 0.0, 1.0]`

## Example

```fortran
&rivertemp1
  albedo_water(:) = 0.03, 0.2, 0.15, 0.0, 1.0
  pt_a_water(:) = 1.2, 2.0, 1.26, 0.0, 1.0
  emissivity_water(:) = 0.95, 0.99, 0.96, 0.0, 1.0
  turb_heat_ex_coeff(:) = 10.0, 50.0, 20.0, 0.0, 1.0
/
```

