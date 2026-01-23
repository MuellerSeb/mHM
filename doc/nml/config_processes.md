# Processes configuration {#config_processes}

[TOC]

Configuration for process case selection in mHM.

**Namelist**: `config_processes`

## Fields

| Name | Type | Required | Info |
| --- | --- | --- | --- |
| [process_case](#process_case) | integer array | no | Process cases |

## Field details

### process_case

Process cases `process_case`

Selection of process cases for mHM simulation.
Index mapping for process_case(i):
1. interception
  - 1: maximum Interception
2. snow
  - 1: degree-day approach
3. soil moisture
  - 1: Feddes equation for ET reduction, multi-layer infiltration capacity approach, Brooks-Corey like
  - 2: Jarvis equation for ET reduction, multi-layer infiltration capacity approach, Brooks-Corey like
  - 3: Jarvis equation for ET reduction and global FC dependency on root fraction coefficient
  - 4: Feddes equation for ET reduction and global FC dependency on root fraction coefficient
4. direct runoff
  - 1: linear reservoir exceedance approach
5. potential evapotranspiration (PET)
  - -1: PET is input, LAI driven correction
  - 0: PET is input, aspect driven correction
  - 1: Hargreaves-Samani method
  - 2: Priestley-Taylor method
  - 3: Penman-Monteith method
6. interflow
  - 1: storage reservoir with one outflow threshold and nonlinear response
7. percolation
  - 1: GW assumed as linear reservoir
8. routing
  - 0: deactivated
  - 1: Muskingum approach
  - 2: adaptive timestep, constant celerity
  - 3: adaptive timestep, spatially varying celerity
9. baseflow
  - 1: recession parameters (not regionalized yet)
10. ground albedo of cosmic-ray neutrons (work in progress, do not use for research)
  - 0: deactivated
  - 1: inverse N0 based on Desilets et al. 2010
  - 2: COSMIC forward operator by Shuttleworth et al. 2013
11. river temperature routing (needs routing)
  - 0: deactivated
  - 1: following: Beek et al., 2012

Summary:
- Type: `integer(i4), dimension(11)`
- Required: no
- Default: `0`

## Example

```fortran
&config_processes
  process_case(:) = 0
/
```

