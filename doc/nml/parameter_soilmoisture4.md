# Soil moisture - Case 4 {#soilmoisture4}

[TOC]

Feddes equation for ET reduction and global FC dependency on root fraction coefficient

**Namelist**: `soilmoisture4`

## Fields

| Name | Type | Required | Info |
| --- | --- | --- | --- |
| [orgMatterContent_forest](#orgmattercontent_forest) | real array | yes | Organic matter content for forest |
| [orgMatterContent_impervious](#orgmattercontent_impervious) | real array | yes | Organic matter content for impervious |
| [orgMatterContent_pervious](#orgmattercontent_pervious) | real array | yes | Organic matter content for pervious |
| [PTF_lower66_5_constant](#ptf_lower66_5_constant) | real array | yes | Zacharias PTF parameters below 66.5 % sand content |
| [PTF_lower66_5_clay](#ptf_lower66_5_clay) | real array | yes | Multiplier for clay constant below 66.5 % sand content |
| [PTF_lower66_5_Db](#ptf_lower66_5_db) | real array | yes | Multiplier for mineral bulk density below 66.5 % sand content |
| [PTF_higher66_5_constant](#ptf_higher66_5_constant) | real array | yes | Zacharias PTF parameters above 66.5 % sand content |
| [PTF_higher66_5_clay](#ptf_higher66_5_clay) | real array | yes | Multiplier for clay constant above 66.5 % sand content |
| [PTF_higher66_5_Db](#ptf_higher66_5_db) | real array | yes | Multiplier for mineral bulk density above 66.5 % sand content |
| [PTF_Ks_constant](#ptf_ks_constant) | real array | yes | PTF constant for saturated hydraulic conductivity |
| [PTF_Ks_sand](#ptf_ks_sand) | real array | yes | Multiplier for sand for saturated hydraulic conductivity |
| [PTF_Ks_clay](#ptf_ks_clay) | real array | yes | Multiplier for clay for saturated hydraulic conductivity |
| [PTF_Ks_curveSlope](#ptf_ks_curveslope) | real array | no | Unit conversion factor |
| [rootFractionCoefficient_forest](#rootfractioncoefficient_forest) | real array | yes | Root fraction coefficient for forest |
| [rootFractionCoefficient_impervious](#rootfractioncoefficient_impervious) | real array | yes | Root fraction coefficient for impervious |
| [rootFractionCoefficient_pervious](#rootfractioncoefficient_pervious) | real array | yes | Root fraction coefficient for pervious |
| [infiltrationShapeFactor](#infiltrationshapefactor) | real array | yes | Infiltration shape factor |
| [rootFractionCoefficient_sand](#rootfractioncoefficient_sand) | real array | yes | Root fraction coefficient for sand |
| [rootFractionCoefficient_clay](#rootfractioncoefficient_clay) | real array | yes | Root fraction coefficient for clay |
| [FCmin_glob](#fcmin_glob) | real array | yes | Field capacity minimum global |
| [FCdelta_glob](#fcdelta_glob) | real array | yes | Field capacity delta global |

## Field details

### orgMatterContent_forest

Organic matter content for forest `orgMatterContent_forest`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.0, 20.0, 3.4, 1.0, 1.0]`

### orgMatterContent_impervious

Organic matter content for impervious `orgMatterContent_impervious`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.0, 1.0, 0.1, 1.0, 1.0]`

### orgMatterContent_pervious

Organic matter content for pervious `orgMatterContent_pervious`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.0, 4.0, 0.6, 1.0, 1.0]`

### PTF_lower66_5_constant

Zacharias PTF parameters below 66.5 % sand content `PTF_lower66_5_constant`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.6462, 0.9506, 0.76, 1.0, 1.0]`

### PTF_lower66_5_clay

Multiplier for clay constant below 66.5 % sand content `PTF_lower66_5_clay`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.0001, 0.0029, 0.0009, 1.0, 1.0]`

### PTF_lower66_5_Db

Multiplier for mineral bulk density below 66.5 % sand content `PTF_lower66_5_Db`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[-0.3727, -0.1871, -0.264, 1.0, 1.0]`

### PTF_higher66_5_constant

Zacharias PTF parameters above 66.5 % sand content `PTF_higher66_5_constant`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.5358, 1.1232, 0.89, 1.0, 1.0]`

### PTF_higher66_5_clay

Multiplier for clay constant above 66.5 % sand content `PTF_higher66_5_clay`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[-0.0055, 0.0049, -0.001, 1.0, 1.0]`

### PTF_higher66_5_Db

Multiplier for mineral bulk density above 66.5 % sand content `PTF_higher66_5_Db`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[-0.5513, -0.0913, -0.324, 1.0, 1.0]`

### PTF_Ks_constant

PTF constant for saturated hydraulic conductivity `PTF_Ks_constant`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[-1.2, -0.285, -0.585, 1.0, 1.0]`

### PTF_Ks_sand

Multiplier for sand for saturated hydraulic conductivity `PTF_Ks_sand`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.006, 0.026, 0.0125, 1.0, 1.0]`

### PTF_Ks_clay

Multiplier for clay for saturated hydraulic conductivity `PTF_Ks_clay`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.003, 0.013, 0.0063, 1.0, 1.0]`

### PTF_Ks_curveSlope

Unit conversion factor `PTF_Ks_curveSlope`

Unit conversion factor from inch/h to cm/d -> should not be here

Summary:
- Type: `real(dp), dimension(5)`
- Required: no
- Default: `[60.96, 60.96, 60.96, 0.0, 1.0]`

### rootFractionCoefficient_forest

Root fraction coefficient for forest `rootFractionCoefficient_forest`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.97, 0.985, 0.975, 1.0, 1.0]`

### rootFractionCoefficient_impervious

Root fraction coefficient for impervious `rootFractionCoefficient_impervious`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.97, 0.985, 0.975, 1.0, 1.0]`

### rootFractionCoefficient_pervious

Root fraction coefficient for pervious `rootFractionCoefficient_pervious`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.97, 0.985, 0.975, 1.0, 1.0]`

### infiltrationShapeFactor

Infiltration shape factor `infiltrationShapeFactor`

Shape factor for partitioning effective precipitation into
runoff and infiltration based on soil wetness [-]

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[1.0, 4.0, 1.75, 1.0, 1.0]`

### rootFractionCoefficient_sand

Root fraction coefficient for sand `rootFractionCoefficient_sand`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.001, 0.09, 0.09, 1.0, 1.0]`

### rootFractionCoefficient_clay

Root fraction coefficient for clay `rootFractionCoefficient_clay`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.9, 0.999, 0.98, 1.0, 1.0]`

### FCmin_glob

Field capacity minimum global `FCmin_glob`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.1, 0.2, 0.15, 0.0, 1.0]`

### FCdelta_glob

Field capacity delta global `FCdelta_glob`

Summary:
- Type: `real(dp), dimension(5)`
- Required: yes
- Examples: `[0.1, 0.4, 0.25, 0.0, 1.0]`

## Example

```fortran
&soilmoisture4
  orgMatterContent_forest(:) = 0.0, 20.0, 3.4, 1.0, 1.0
  orgMatterContent_impervious(:) = 0.0, 1.0, 0.1, 1.0, 1.0
  orgMatterContent_pervious(:) = 0.0, 4.0, 0.6, 1.0, 1.0
  PTF_lower66_5_constant(:) = 0.6462, 0.9506, 0.76, 1.0, 1.0
  PTF_lower66_5_clay(:) = 0.0001, 0.0029, 0.0009, 1.0, 1.0
  PTF_lower66_5_Db(:) = -0.3727, -0.1871, -0.264, 1.0, 1.0
  PTF_higher66_5_constant(:) = 0.5358, 1.1232, 0.89, 1.0, 1.0
  PTF_higher66_5_clay(:) = -0.0055, 0.0049, -0.001, 1.0, 1.0
  PTF_higher66_5_Db(:) = -0.5513, -0.0913, -0.324, 1.0, 1.0
  PTF_Ks_constant(:) = -1.2, -0.285, -0.585, 1.0, 1.0
  PTF_Ks_sand(:) = 0.006, 0.026, 0.0125, 1.0, 1.0
  PTF_Ks_clay(:) = 0.003, 0.013, 0.0063, 1.0, 1.0
  PTF_Ks_curveSlope(:) = 60.96, 60.96, 60.96, 0.0, 1.0
  rootFractionCoefficient_forest(:) = 0.97, 0.985, 0.975, 1.0, 1.0
  rootFractionCoefficient_impervious(:) = 0.97, 0.985, 0.975, 1.0, 1.0
  rootFractionCoefficient_pervious(:) = 0.97, 0.985, 0.975, 1.0, 1.0
  infiltrationShapeFactor(:) = 1.0, 4.0, 1.75, 1.0, 1.0
  rootFractionCoefficient_sand(:) = 0.001, 0.09, 0.09, 1.0, 1.0
  rootFractionCoefficient_clay(:) = 0.9, 0.999, 0.98, 1.0, 1.0
  FCmin_glob(:) = 0.1, 0.2, 0.15, 0.0, 1.0
  FCdelta_glob(:) = 0.1, 0.4, 0.25, 0.0, 1.0
/
```

