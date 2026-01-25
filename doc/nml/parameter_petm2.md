# PET - Case -2 {#petm2}

[TOC]

Parameters for PET (case -2 - aspect correction).

**Namelist**: `petm2`

## Fields

| Name | Type | Required | Info |
| --- | --- | --- | --- |
| [minCorrectionFactorPET](#mincorrectionfactorpet) | real array | yes | minimum factor for PET correction with aspect |
| [maxCorrectionFactorPET](#maxcorrectionfactorpet) | real array | yes | maximum factor for PET correction with aspect |
| [aspectTresholdPET](#aspecttresholdpet) | real array | yes | aspect threshold |

## Field details

### minCorrectionFactorPET

minimum factor for PET correction with aspect `minCorrectionFactorPET`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.7, 1.3, 0.9, 1.0, 1.0]`

### maxCorrectionFactorPET

maximum factor for PET correction with aspect `maxCorrectionFactorPET`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.0, 0.2, 0.1, 1.0, 1.0]`

### aspectTresholdPET

aspect threshold `aspectTresholdPET`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[160.0, 200.0, 180.0, 1.0, 1.0]`

## Example

```fortran
&petm2
  minCorrectionFactorPET(:) = 0.7, 1.3, 0.9, 1.0, 1.0
  maxCorrectionFactorPET(:) = 0.0, 0.2, 0.1, 1.0, 1.0
  aspectTresholdPET(:) = 160.0, 200.0, 180.0, 1.0, 1.0
/
```

