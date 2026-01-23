# Routing - Case 2 {#routing2}

[TOC]

Parameters for routing (case 2 - constant celerity).

**Namelist**: `routing2`

## Fields

| Name | Type | Required | Info |
| --- | --- | --- | --- |
| [streamflow_celerity](#streamflow_celerity) | real array | yes | Streamflow celerity |

## Field details

### streamflow_celerity

Streamflow celerity `streamflow_celerity`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.1, 15.0, 1.5, 0.0, 1.0]`

## Example

```fortran
&routing2
  streamflow_celerity(:) = 0.1, 15.0, 1.5, 0.0, 1.0
/
```

