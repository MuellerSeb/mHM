# PET - Case 2 {#pet2}

[TOC]

Parameters for PET (case 2 - Priestley-Taylor).

**Namelist**: `pet2`

## Fields

| Name | Type | Required | Info |
| --- | --- | --- | --- |
| [PriestleyTaylorCoeff](#priestleytaylorcoeff) | real array | yes | Priestley-Taylor coefficient |
| [PriestleyTaylorLAIcorr](#priestleytaylorlaicorr) | real array | yes | Priestley-Taylor LAI correction factor |

## Field details

### PriestleyTaylorCoeff

Priestley-Taylor coefficient `PriestleyTaylorCoeff`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.75, 1.75, 1.19, 1.0, 1.0]`

### PriestleyTaylorLAIcorr

Priestley-Taylor LAI correction factor `PriestleyTaylorLAIcorr`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[-0.5, 0.2, 0.058, 1.0, 1.0]`

## Example

```fortran
&pet2
  PriestleyTaylorCoeff(:) = 0.75, 1.75, 1.19, 1.0, 1.0
  PriestleyTaylorLAIcorr(:) = -0.5, 0.2, 0.058, 1.0, 1.0
/
```

