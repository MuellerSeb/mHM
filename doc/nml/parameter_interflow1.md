# Interflow - Case 1 {#interflow1}

[TOC]

Parameters for interflow1.

**Namelist**: `interflow1`

## Fields

| Name | Type | Required | Info |
| --- | --- | --- | --- |
| [interflowStorageCapacityFactor](#interflowstoragecapacityfactor) | real array | yes | Storage capacity factor for interflow |
| [interflowRecession_slope](#interflowrecession_slope) | real array | yes | Multiplier for slope to derive interflow recession constant |
| [fastInterflowRecession_forest](#fastinterflowrecession_forest) | real array | yes | Multiplier for forest to derive fast interflow recession constant |
| [slowInterflowRecession_Ks](#slowinterflowrecession_ks) | real array | yes | Multiplier for variability of saturated hydraulic conductivity to derive slow interflow recession constant |
| [exponentSlowInterflow](#exponentslowinterflow) | real array | yes | Multiplier for variability of saturated hydraulic conductivity to derive slow interflow exponent |

## Field details

### interflowStorageCapacityFactor

Storage capacity factor for interflow `interflowStorageCapacityFactor`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[75.0, 200.0, 85.0, 1.0, 1.0]`

### interflowRecession_slope

Multiplier for slope to derive interflow recession constant `interflowRecession_slope`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.0, 10.0, 7.0, 1.0, 1.0]`

### fastInterflowRecession_forest

Multiplier for forest to derive fast interflow recession constant `fastInterflowRecession_forest`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[1.0, 3.0, 1.5, 1.0, 1.0]`

### slowInterflowRecession_Ks

Multiplier for variability of saturated hydraulic conductivity to derive slow interflow recession constant `slowInterflowRecession_Ks`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[1.0, 30.0, 15.0, 1.0, 1.0]`

### exponentSlowInterflow

Multiplier for variability of saturated hydraulic conductivity to derive slow interflow exponent `exponentSlowInterflow`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.05, 0.3, 0.125, 1.0, 1.0]`

## Example

```fortran
&interflow1
  interflowStorageCapacityFactor(:) = 75.0, 200.0, 85.0, 1.0, 1.0
  interflowRecession_slope(:) = 0.0, 10.0, 7.0, 1.0, 1.0
  fastInterflowRecession_forest(:) = 1.0, 3.0, 1.5, 1.0, 1.0
  slowInterflowRecession_Ks(:) = 1.0, 30.0, 15.0, 1.0, 1.0
  exponentSlowInterflow(:) = 0.05, 0.3, 0.125, 1.0, 1.0
/
```

