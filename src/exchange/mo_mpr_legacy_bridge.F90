!> \file    mo_mpr_legacy_bridge.f90
!> \copydoc mo_mpr_legacy_bridge

!> \brief   Bridge helpers between the exchange MPR container and legacy MPR routines.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Mar 2026
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_exchange
#include "logging.h"
module mo_mpr_legacy_bridge
  use mo_logging
  use mo_kind, only: i4, dp
  use mo_grid_scaler, only: scaler_t, up_fraction, up_h_mean
  use mo_string_utils, only: n2s => num2str

  implicit none
  private

  public :: mpr_bridge_land_cover_fraction
  public :: mpr_bridge_snow_param
  public :: mpr_bridge_pet_lai

contains

  !> \brief Upscale land-cover fractions from packed L0 ids to packed L1 fractions.
  subroutine mpr_bridge_land_cover_fraction(upscaler, land_cover_l0, frac_sealed_cityarea, forest_l1, sealed_l1, pervious_l1)
    class(scaler_t), intent(inout), target :: upscaler
    integer(i4), dimension(:), intent(in) :: land_cover_l0
    real(dp), intent(in) :: frac_sealed_cityarea
    real(dp), dimension(:), intent(out) :: forest_l1
    real(dp), dimension(:), intent(out) :: sealed_l1
    real(dp), dimension(:), intent(out) :: pervious_l1

    call upscaler%execute(land_cover_l0, forest_l1, upscaling_operator=up_fraction, class_id=1_i4)
    call upscaler%execute(land_cover_l0, sealed_l1, upscaling_operator=up_fraction, class_id=2_i4)

    sealed_l1 = frac_sealed_cityarea * sealed_l1
    pervious_l1 = 1.0_dp - forest_l1 - sealed_l1
  end subroutine mpr_bridge_land_cover_fraction

  !> \brief Bridge snow parameter generation through the legacy degree-day routine.
  subroutine mpr_bridge_snow_param(param, forest_l1, sealed_l1, pervious_l1, thresh_temp_l1, degday_dry_l1, degday_inc_l1, degday_max_l1)
    real(dp), dimension(:), intent(in) :: param
    real(dp), dimension(:), intent(in) :: forest_l1
    real(dp), dimension(:), intent(in) :: sealed_l1
    real(dp), dimension(:), intent(in) :: pervious_l1
    real(dp), dimension(:), intent(out) :: thresh_temp_l1
    real(dp), dimension(:), intent(out) :: degday_dry_l1
    real(dp), dimension(:), intent(out) :: degday_inc_l1
    real(dp), dimension(:), intent(out) :: degday_max_l1
    real(dp) :: degday_dry_forest
    real(dp) :: degday_dry_sealed
    real(dp) :: degday_dry_pervious
    real(dp) :: degday_max_forest
    real(dp) :: degday_max_sealed
    real(dp) :: degday_max_pervious

    if (size(param) < 8_i4) then
      log_fatal(*) "MPR bridge: snow parameter set must contain 8 values, got ", n2s(size(param, kind=i4)), "."
      error stop 1
    end if

    degday_dry_forest = param(2)
    degday_dry_sealed = param(2) + param(4) + param(3)
    degday_dry_pervious = param(2) + param(4)
    degday_max_forest = param(2) + param(6)
    degday_max_sealed = param(2) + param(4) + param(3) + param(7)
    degday_max_pervious = param(2) + param(4) + param(8)

    thresh_temp_l1 = param(1)
    degday_inc_l1 = param(5)
    degday_dry_l1 = degday_dry_forest * forest_l1 + degday_dry_sealed * sealed_l1 + degday_dry_pervious * pervious_l1
    degday_max_l1 = degday_max_forest * forest_l1 + degday_max_sealed * sealed_l1 + degday_max_pervious * pervious_l1
  end subroutine mpr_bridge_snow_param

  !> \brief Bridge PET-LAI correction with legacy-compatible formulas on top of the new scaler.
  subroutine mpr_bridge_pet_lai(param, land_cover_l0, lai_l0, upscaler, pet_fac_lai_l1)
    real(dp), dimension(:), intent(in) :: param
    integer(i4), dimension(:), intent(in) :: land_cover_l0
    real(dp), dimension(:), intent(in) :: lai_l0
    class(scaler_t), intent(inout), target :: upscaler
    real(dp), dimension(:), intent(out) :: pet_fac_lai_l1
    real(dp), allocatable :: pet_fac_lai_l0(:)
    integer(i4) :: i
    integer(i4) :: land_cover_id

    if (size(param) < 5_i4) then
      log_fatal(*) "MPR bridge: PET-LAI parameter set must contain 5 values, got ", n2s(size(param, kind=i4)), "."
      error stop 1
    end if
    if (size(land_cover_l0) /= size(lai_l0)) then
      log_fatal(*) "MPR bridge: PET-LAI inputs must have matching packed L0 sizes."
      error stop 1
    end if

    allocate(pet_fac_lai_l0(size(land_cover_l0)))
    do i = 1_i4, size(land_cover_l0)
      land_cover_id = land_cover_l0(i)
      select case (land_cover_id)
        case (1_i4)
          pet_fac_lai_l0(i) = param(1) + (param(4) * (1.0_dp - exp(param(5) * lai_l0(i))))
        case (2_i4)
          pet_fac_lai_l0(i) = param(2) + (param(4) * (1.0_dp - exp(param(5) * lai_l0(i))))
        case (3_i4)
          pet_fac_lai_l0(i) = param(3) + (param(4) * (1.0_dp - exp(param(5) * lai_l0(i))))
        case default
          log_fatal(*) "MPR bridge: unsupported land-cover ID ", n2s(land_cover_id), &
            " for PET-LAI correction at packed L0 cell ", n2s(i), "."
          error stop 1
      end select
    end do

    call upscaler%execute(pet_fac_lai_l0, pet_fac_lai_l1, upscaling_operator=up_h_mean)
    deallocate(pet_fac_lai_l0)
  end subroutine mpr_bridge_pet_lai

end module mo_mpr_legacy_bridge
