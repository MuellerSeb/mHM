# Neutrons - Case 1 {#neutrons1}

[TOC]

Parameters for neutrons (case 1 - Desilets).
Ground albedo neutrons - DESILET version.
THIS IS WORK IN PROGRESS, DO NOT USE FOR RESEARCH

**Namelist**: `neutrons1`

## Fields

| Name | Type | Required | Info |
| --- | --- | --- | --- |
| [Desilets_N0](#desilets_n0) | real array | yes | Desilets N0 dry count |
| [Desilets_LW0](#desilets_lw0) | real array | yes | Desilets LW0 parameter |
| [Desilets_LW1](#desilets_lw1) | real array | yes | Desilets LW1 parameter |

## Field details

### Desilets_N0

Desilets N0 dry count `Desilets_N0`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[300.0, 2000.0, 1500.0, 0.0, 1.0]`

### Desilets_LW0

Desilets LW0 parameter `Desilets_LW0`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.0, 0.2, 0.1783, 0.0, 1.0]`

### Desilets_LW1

Desilets LW1 parameter `Desilets_LW1`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.0, 0.05, 0.0, 0.0, 1.0]`

## Example

```fortran
&neutrons1
  Desilets_N0(:) = 300.0, 2000.0, 1500.0, 0.0, 1.0
  Desilets_LW0(:) = 0.0, 0.2, 0.1783, 0.0, 1.0
  Desilets_LW1(:) = 0.0, 0.05, 0.0, 0.0, 1.0
/
```

