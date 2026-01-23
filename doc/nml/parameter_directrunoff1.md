# Direct runoff - Case 1 {#directrunoff1}

[TOC]

Parameters for Direct sealed area runoff.

**Namelist**: `directrunoff1`

## Fields

| Name | Type | Required | Info |
| --- | --- | --- | --- |
| [imperviousStorageCapacity](#imperviousstoragecapacity) | real array | yes | Capacity of impervious storage [mm] |

## Field details

### imperviousStorageCapacity

Capacity of impervious storage [mm] `imperviousStorageCapacity`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.0, 5.0, 0.5, 1.0, 1.0]`

## Example

```fortran
&directrunoff1
  imperviousStorageCapacity(:) = 0.0, 5.0, 0.5, 1.0, 1.0
/
```

