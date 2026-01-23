# mRM configuration {#config_mrm}

[TOC]

Configuration for the multi-scale routing model (mRM) in mHM.

**Namelist**: `config_mrm`

## Fields

| Name | Type | Required | Info |
| --- | --- | --- | --- |
| [resolution](#resolution) | real array | yes | mRM resolution (L3) |
| [river_net_order_root_based](#river_net_order_root_based) | logical array | no | Flag for root based river network ordering. |
| [river_net_omp_level_min](#river_net_omp_level_min) | integer array | no | Minimum level size for OpenMP parallelization. |
| [scc_gauges_path](#scc_gauges_path) | string array | no | Path for SCC gauges NetCDF file. |
| [output_path](#output_path) | string array | no | Path for output file. |
| [output_node_path](#output_node_path) | string array | no | Path for node based output file. |
| [read_restart](#read_restart) | logical array | no | Read restart |
| [read_restart_fluxes](#read_restart_fluxes) | logical array | no | Read restart fluxes |
| [restart_input_path](#restart_input_path) | string array | no | Restart input path |
| [write_restart](#write_restart) | logical array | no | Write restart |
| [restart_output_path](#restart_output_path) | string array | no | Restart output path |
| [albedo_water](#albedo_water) | real | no | Albedo of open water |
| [pt_a_water](#pt_a_water) | real | no | Priestley-Taylor alpha |
| [emissivity_water](#emissivity_water) | real | no | Emissivity of water |
| [turb_heat_ex_coeff](#turb_heat_ex_coeff) | real | no | Turbulent heat exchange coefficient |
| [max_iter](#max_iter) | integer | no | Max number of iterations |
| [delta_iter](#delta_iter) | real | no | Convergence criterion iteration |
| [step_iter](#step_iter) | real | no | Maximal step in iteration |

## Field details

### resolution

mRM resolution (L3) `resolution`

Resolution of the mRM routing grid, level 3.

Summary:
- Type: `real(dp), dimension(max_domains)`
- Flexible tail dims: 1
- Required: yes
- Minimum: `> 0.0`
- Examples: `[0.1]`

### river_net_order_root_based

Flag for root based river network ordering. `river_net_order_root_based`

Flag to indicate if the river network is ordered in root based order.
If false, the ordering is leaf based.
Root based ordering results in more equal distributed level sizes for parallelization.
Leaf based ordering has huge levels of nodes at the headwaters, which can lead to load balancing issues.

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`
- Examples: `[.true.]`

### river_net_omp_level_min

Minimum level size for OpenMP parallelization. `river_net_omp_level_min`

Minimum level size in the river network where OpenMP parallelization starts.
Levels smaller than this size are always computed serially.
This can be used to avoid parallelization overhead for small levels of the river network,
especially when ordering is leaf based.
By default: threads * 8 (indicated by 0)

Summary:
- Type: `integer(i4), dimension(max_domains)`
- Required: no
- Default: `0`
- Minimum: `>= 0`
- Examples: `[100]`

### scc_gauges_path

Path for SCC gauges NetCDF file. `scc_gauges_path`

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no
- Examples: `["scc_gauges.nc"]`

### output_path

Path for output file. `output_path`

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no
- Examples: `["mrm_output.nc"]`

### output_node_path

Path for node based output file. `output_node_path`

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no
- Examples: `["mrm_node_output.nc"]`

### read_restart

Read restart `read_restart`

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### read_restart_fluxes

Read restart fluxes `read_restart_fluxes`

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.true.`

### restart_input_path

Restart input path `restart_input_path`

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no
- Examples: `["mrm_restart_in.nc"]`

### write_restart

Write restart `write_restart`

Summary:
- Type: `logical, dimension(max_domains)`
- Required: no
- Default: `.false.`

### restart_output_path

Restart output path `restart_output_path`

Summary:
- Type: `character(len=buf), dimension(max_domains)`
- Flexible tail dims: 1
- Required: no
- Examples: `["mrm_restart_out.nc"]`

### albedo_water

Albedo of open water `albedo_water`

Albedo of open water bodies.
(0.15 for tilt angle between 10 and 20 degrees -> Wanders et.al. 2019)

Summary:
- Type: `real(dp)`
- Required: no
- Default: `0.15`

### pt_a_water

Priestley-Taylor alpha `pt_a_water`

Priestley-Taylor alpha parameter for PET on open water.
(1.26 -> Gordon Bonan 2015)

Summary:
- Type: `real(dp)`
- Required: no
- Default: `1.26`

### emissivity_water

Emissivity of water `emissivity_water`

Emissivity of water.
(0.96 -> Wanders et.al. 2019)

Summary:
- Type: `real(dp)`
- Required: no
- Default: `0.96`

### turb_heat_ex_coeff

Turbulent heat exchange coefficient `turb_heat_ex_coeff`

Turbulent heat exchange coefficient.
(20.0 -> Wanders et.al. 2019)

Summary:
- Type: `real(dp)`
- Required: no
- Default: `20.0`

### max_iter

Max number of iterations `max_iter`

Maximum number of iterations for the iterative solver for river temperature.

Summary:
- Type: `integer(i4)`
- Required: no
- Default: `50`

### delta_iter

Convergence criterion iteration `delta_iter`

Convergence criteria for iterative solver for resulting river temperature.
Given as difference for iteratively estimated temperature in K

Summary:
- Type: `real(dp)`
- Required: no
- Default: `0.01`

### step_iter

Maximal step in iteration `step_iter`

Maximal step allowed in iteration for river temperature in K.
Used to avoid divergence of the iterative solver.

Summary:
- Type: `real(dp)`
- Required: no
- Default: `5.0`

## Example

```fortran
&config_mrm
  resolution(:) = 0.1
  river_net_order_root_based(:) = .true.
  river_net_omp_level_min(:) = 100
  scc_gauges_path(:) = "scc_gauges.nc"
  output_path(:) = "mrm_output.nc"
  output_node_path(:) = "mrm_node_output.nc"
  read_restart(:) = .false.
  read_restart_fluxes(:) = .true.
  restart_input_path(:) = "mrm_restart_in.nc"
  write_restart(:) = .false.
  restart_output_path(:) = "mrm_restart_out.nc"
  albedo_water = 0.15
  pt_a_water = 1.26
  emissivity_water = 0.96
  turb_heat_ex_coeff = 20.0
  max_iter = 50
  delta_iter = 0.01
  step_iter = 5.0
/
```

