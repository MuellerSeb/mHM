# mRM configuration {#config_mrm}

[TOC]

Configuration for the multi-scale routing model (mRM) in mHM.

**Namelist**: `config_mrm`

## Fields

| Name | Type | Required | Info |
| --- | --- | --- | --- |
| `resolution` | integer array | yes | mRM resolution |
| `output_path` | string array | no | Output path |
| `read_restart` | logical array | no | Read restart |
| `read_restart_fluxes` | logical array | no | Read restart fluxes |
| `restart_input_path` | string array | no | Restart input path |
| `write_restart` | logical array | no | Write restart |
| `restart_output_path` | string array | no | Restart output path |
| `albedo_water` | real | no | Albedo of open water |
| `pt_a_water` | real | no | Priestley-Taylor alpha |
| `emissivity_water` | real | no | Emissivity of water |
| `turb_heat_ex_coeff` | real | no | Turbulent heat exchange coefficient |
| `max_iter` | integer | no | Max number of iterations |
| `delta_iter` | real | no | Convergence criterion iteration |
| `step_iter` | real | no | Maximal step in iteration |

## Field details

### `resolution` - mRM resolution

Resolution of the mRM routing grid.

Summary:
- Type: `integer(i4), dimension(max_domains)`
- Flexible tail dims: 1
- Required: yes

### `output_path` - Output path

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `read_restart` - Read restart

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### `read_restart_fluxes` - Read restart fluxes

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.true.`

### `restart_input_path` - Restart input path

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `write_restart` - Write restart

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### `restart_output_path` - Restart output path

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

### `albedo_water` - Albedo of open water

Albedo of open water bodies.
(0.15 for tilt angle between 10 and 20 degrees -> Wanders et.al. 2019)

Summary:
- Type: `real(dp)`
- Required: no
- Default: `0.15`

### `pt_a_water` - Priestley-Taylor alpha

Priestley-Taylor alpha parameter for PET on open water.
(1.26 -> Gordon Bonan 2015)

Summary:
- Type: `real(dp)`
- Required: no
- Default: `1.26`

### `emissivity_water` - Emissivity of water

Emissivity of water.
(0.96 -> Wanders et.al. 2019)

Summary:
- Type: `real(dp)`
- Required: no
- Default: `0.96`

### `turb_heat_ex_coeff` - Turbulent heat exchange coefficient

Turbulent heat exchange coefficient.
(20.0 -> Wanders et.al. 2019)

Summary:
- Type: `real(dp)`
- Required: no
- Default: `20.0`

### `max_iter` - Max number of iterations

Maximum number of iterations for the iterative solver for river temperature.

Summary:
- Type: `integer(i4)`
- Required: no
- Default: `50`

### `delta_iter` - Convergence criterion iteration

Convergence criteria for iterative solver for resulting river temperature.
Given as difference for iteratively estimated temperature in K

Summary:
- Type: `real(dp)`
- Required: no
- Default: `0.01`

### `step_iter` - Maximal step in iteration

Maximal step allowed in iteration for river temperature in K.
Used to avoid divergence of the iterative solver.

Summary:
- Type: `real(dp)`
- Required: no
- Default: `5.0`

## Example

```fortran
&config_mrm
  resolution(:) = 0
  output_path(:) = ''
  read_restart(:) = .false.
  read_restart_fluxes(:) = .true.
  restart_input_path(:) = ''
  write_restart(:) = .false.
  restart_output_path(:) = ''
  albedo_water = 0.15
  pt_a_water = 1.26
  emissivity_water = 0.96
  turb_heat_ex_coeff = 20.0
  max_iter = 50
  delta_iter = 0.01
  step_iter = 5.0
/
```

