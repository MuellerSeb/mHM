# PET - Case 1 {#pet1}

[TOC]

Parameters for PET (case 1 - Hargreaves-Samani).

**Namelist**: `pet1`

## Fields

| Name | Type | Required | Info |
| --- | --- | --- | --- |
| [minCorrectionFactorPET](#mincorrectionfactorpet) | real array | yes | minimum correction factor for PET |
| [maxCorrectionFactorPET](#maxcorrectionfactorpet) | real array | yes | maximum correction factor for PET |
| [aspectTresholdPET](#aspecttresholdpet) | real array | yes | aspect threshold for PET |
| [HargreavesSamaniCoeff](#hargreavessamanicoeff) | real array | yes | Hargreaves-Samani coefficient |

## Field details

### minCorrectionFactorPET

minimum correction factor for PET `minCorrectionFactorPET`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.7, 1.3, 0.93, 1.0, 1.0]`

### maxCorrectionFactorPET

maximum correction factor for PET `maxCorrectionFactorPET`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.0, 0.2, 0.19, 1.0, 1.0]`

### aspectTresholdPET

aspect threshold for PET `aspectTresholdPET`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[160.0, 200.0, 171.0, 1.0, 1.0]`

### HargreavesSamaniCoeff

Hargreaves-Samani coefficient `HargreavesSamaniCoeff`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.0016, 0.003, 0.0023, 1.0, 1.0]`

## Example

```fortran
&pet1
  minCorrectionFactorPET(:) = 0.7, 1.3, 0.93, 1.0, 1.0
  maxCorrectionFactorPET(:) = 0.0, 0.2, 0.19, 1.0, 1.0
  aspectTresholdPET(:) = 160.0, 200.0, 171.0, 1.0, 1.0
  HargreavesSamaniCoeff(:) = 0.0016, 0.003, 0.0023, 1.0, 1.0
/
```

