# Neutrons - Case 2 {#neutrons2}

[TOC]

Parameters for neutrons (case 2 - COSMIC).
Ground albedo neutrons - COSMIC version.
THIS IS WORK IN PROGRESS, DO NOT USE FOR RESEARCH

**Namelist**: `neutrons2`

## Fields

| Name | Type | Required | Info |
| --- | --- | --- | --- |
| [COSMIC_N0](#cosmic_n0) | real array | yes | Cosmic N0 parameter |
| [COSMIC_N1](#cosmic_n1) | real array | yes | Cosmic N1 parameter |
| [COSMIC_N2](#cosmic_n2) | real array | yes | Cosmic N2 parameter |
| [COSMIC_alpha0](#cosmic_alpha0) | real array | yes | Cosmic alpha0 parameter |
| [COSMIC_alpha1](#cosmic_alpha1) | real array | yes | Cosmic alpha1 parameter |
| [COSMIC_L30](#cosmic_l30) | real array | yes | Cosmic L30 parameter |
| [COSMIC_L31](#cosmic_l31) | real array | yes | Cosmic L31 parameter |
| [COSMIC_LW0](#cosmic_lw0) | real array | yes | Cosmic LW0 parameter |
| [COSMIC_LW1](#cosmic_lw1) | real array | yes | Cosmic LW1 parameter |

## Field details

### COSMIC_N0

Cosmic N0 parameter `COSMIC_N0`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[300.0, 2000.0, 1500.0, 0.0, 1.0]`

### COSMIC_N1

Cosmic N1 parameter `COSMIC_N1`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.01, 10.0, 1.0, 0.0, 1.0]`

### COSMIC_N2

Cosmic N2 parameter `COSMIC_N2`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.01, 10.0, 1.0, 0.0, 1.0]`

### COSMIC_alpha0

Cosmic alpha0 parameter `COSMIC_alpha0`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.01, 10.0, 1.0, 0.0, 1.0]`

### COSMIC_alpha1

Cosmic alpha1 parameter `COSMIC_alpha1`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.01, 10.0, 1.0, 0.0, 1.0]`

### COSMIC_L30

Cosmic L30 parameter `COSMIC_L30`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[26.56, 424.78, 106.1942, 0.0, 1.0]`

### COSMIC_L31

Cosmic L31 parameter `COSMIC_L31`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[-118.3, 200.28, 40.9879, 0.0, 1.0]`

### COSMIC_LW0

Cosmic LW0 parameter `COSMIC_LW0`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.0, 0.2, 0.1783, 0.0, 1.0]`

### COSMIC_LW1

Cosmic LW1 parameter `COSMIC_LW1`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.0, 0.05, 0.0, 0.0, 1.0]`

## Example

```fortran
&neutrons2
  COSMIC_N0(:) = 300.0, 2000.0, 1500.0, 0.0, 1.0
  COSMIC_N1(:) = 0.01, 10.0, 1.0, 0.0, 1.0
  COSMIC_N2(:) = 0.01, 10.0, 1.0, 0.0, 1.0
  COSMIC_alpha0(:) = 0.01, 10.0, 1.0, 0.0, 1.0
  COSMIC_alpha1(:) = 0.01, 10.0, 1.0, 0.0, 1.0
  COSMIC_L30(:) = 26.56, 424.78, 106.1942, 0.0, 1.0
  COSMIC_L31(:) = -118.3, 200.28, 40.9879, 0.0, 1.0
  COSMIC_LW0(:) = 0.0, 0.2, 0.1783, 0.0, 1.0
  COSMIC_LW1(:) = 0.0, 0.05, 0.0, 0.0, 1.0
/
```

