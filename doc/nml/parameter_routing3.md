# Routing - Case 3 {#routing3}

[TOC]

Parameters for routing (case 3 - varying celerity).

**Namelist**: `routing3`

## Fields

| Name | Type | Required | Info |
| --- | --- | --- | --- |
| [slope_factor](#slope_factor) | real array | yes | Slope factor |

## Field details

### slope_factor

Slope factor `slope_factor`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.1, 100.0, 30.0, 0.0, 1.0]`

## Example

```fortran
&routing3
  slope_factor(:) = 0.1, 100.0, 30.0, 0.0, 1.0
/
```

