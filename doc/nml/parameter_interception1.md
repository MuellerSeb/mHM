# Interception - Case 1 {#interception1}

[TOC]

Parameters for interception.

**Namelist**: `interception1`

## Fields

| Name | Type | Required | Info |
| --- | --- | --- | --- |
| [canopyInterceptionFactor](#canopyinterceptionfactor) | real array | yes | Multiplier to relate LAI to interception storage |

## Field details

### canopyInterceptionFactor

Multiplier to relate LAI to interception storage `canopyInterceptionFactor`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.15, 0.4, 0.15, 1.0, 1.0]`

## Example

```fortran
&interception1
  canopyInterceptionFactor(:) = 0.15, 0.4, 0.15, 1.0, 1.0
/
```

