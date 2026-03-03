# Percolation - Case 1 {#percolation1}

[TOC]

Parameters for percolation.

**Namelist**: `percolation1`

## Fields

| Name | Type | Required | Info |
| --- | --- | --- | --- |
| [rechargeCoefficient](#rechargecoefficient) | real array | yes | Recharge coefficient |
| [rechargeFactor_karstic](#rechargefactor_karstic) | real array | yes | Recharge factor karstic |
| [gain_loss_GWreservoir_karstic](#gain_loss_gwreservoir_karstic) | real array | yes | Gain/loss GW reservoir karstic |

## Field details

### rechargeCoefficient

Recharge coefficient `rechargeCoefficient`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.0, 50.0, 35.0, 1.0, 1.0]`

### rechargeFactor_karstic

Recharge factor karstic `rechargeFactor_karstic`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[-5.0, 5.0, -1.0, 1.0, 1.0]`

### gain_loss_GWreservoir_karstic

Gain/loss GW reservoir karstic `gain_loss_GWreservoir_karstic`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[1.0, 1.0, 1.0, 0.0, 1.0]`

## Example

```fortran
&percolation1
  rechargeCoefficient(:) = 0.0, 50.0, 35.0, 1.0, 1.0
  rechargeFactor_karstic(:) = -5.0, 5.0, -1.0, 1.0, 1.0
  gain_loss_GWreservoir_karstic(:) = 1.0, 1.0, 1.0, 0.0, 1.0
/
```

