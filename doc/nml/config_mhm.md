# mHM model configuration {#config_mhm}

[TOC]

Configuration for the mHM model setup.

**Namelist**: `config_mhm`

## Fields

| Name | Type | Required | Info |
| --- | --- | --- | --- |
| `resolution` | integer array | no | mHM model resolution |
| `output_path` | string array | no | Output path |
| `read_restart` | logical array | no | Read restart |
| `read_restart_fluxes` | logical array | no | Read restart fluxes |
| `restart_input_path` | string array | no | Restart input path |
| `write_restart` | logical array | no | Write restart |
| `restart_output_path` | string array | no | Restart output path |
| `evap_coeff` | real array | no | Evaporation coefficients |
| `share_evap_coeff` | logical | no | Share evaporation coefficients between domains |

## Field details

### `resolution` - mHM model resolution

Resolution of the mHM model.
(array dimension 1: domain)

Summary:
- Type: `integer(i4), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no

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

### `evap_coeff` - Evaporation coefficients

Monthly free pan evaporation coefficients for free-water surfaces.
(array dimension 1: month, dimension 2: domain)

Summary:
- Type: `real(dp), dimension(12, max_domains)`
- Flexible tail dims: 1
- Required: no
- Examples: `[1.3, 1.2, 0.72, 0.75, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.5]`

### `share_evap_coeff` - Share evaporation coefficients between domains

Share the same evaporation coefficient values between all domains taking the values from the first domain.
If set to false, different evaporation coefficient values can be set for each domain.

Summary:
- Type: `logical`
- Required: no
- Default: `.true.`

## Example

```fortran
&config_mhm
  resolution(:) = 0
  output_path(:) = ''
  read_restart(:) = .false.
  read_restart_fluxes(:) = .true.
  restart_input_path(:) = ''
  write_restart(:) = .false.
  restart_output_path(:) = ''
  evap_coeff(:, 1) = 1.3, 1.2, 0.72, 0.75, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.5
  share_evap_coeff = .true.
/
```

