!> \file    mo_mpr_container.f90
!> \copydoc mo_mpr_container

!> \brief   Module for a mpr process container.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Aug 2025
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_exchange
#include "logging.h"
module mo_mpr_container
  use mo_logging
  use mo_kind, only: i4
  use mo_exchange_type, only: exchange_t
  use mo_string_utils, only: n2s => num2str
  use mo_read_lut, only: read_geoformation_lut
  use mo_message, only: message, error_message
  use nml_config_mpr, only: nml_config_mpr_t, NML_OK

  !> \class   mpr_t
  !> \brief   Class for a single MPR process container.
  type, public :: mpr_t
    type(nml_config_mpr_t) :: config !< configuration of the MPR process container
    type(exchange_t), pointer :: exchange => null() !< exchange container of the domain
    character(:), allocatable :: soil_lut_path !< resolved soil LUT path
    character(:), allocatable :: geo_lut_path !< resolved geology LUT path
    character(:), allocatable :: lai_path !< resolved gridded LAI path
    character(:), allocatable :: lai_lut_path !< resolved LAI LUT path
  contains
    procedure :: configure => mpr_configure
    procedure :: connect => mpr_connect
    procedure :: initialize => mpr_initialize
    procedure :: update => mpr_update
    procedure :: finalize => mpr_finalize
  end type mpr_t

contains

  !> \brief Configure the MPR process container.
  subroutine mpr_configure(self, file)
    class(mpr_t), intent(inout), target :: self
    character(*), intent(in), optional :: file !< file containing the namelists
    integer(i4) :: id(1)
    integer(i4) :: depth_idx(2)
    integer(i4) :: required_depths
    integer(i4) :: i
    character(1024) :: errmsg
    character(:), allocatable :: path
    integer :: status
    log_info(*) "Configure MPR"
    if (present(file)) then
      path = self%exchange%get_path(file) ! get absolute path relative to cwd
      log_info(*) "Read MPR config: ", path
      status = self%config%from_file(file=path, errmsg=errmsg)
      if (status /= NML_OK) then
        log_fatal(*) "Error reading MPR config: ", trim(errmsg)
        error stop 1
      end if
    end if
    if (.not.self%config%is_configured) then
      log_fatal(*) "MPR config not set."
      error stop 1
    end if
    status = self%config%is_valid(errmsg=errmsg)
    if (status /= NML_OK) then
      log_fatal(*) "MPR config not valid: ", trim(errmsg)
      error stop 1
    end if

    id(1) = self%exchange%domain

    status = self%config%is_set("n_horizons", idx=id, errmsg=errmsg)
    if (status /= NML_OK) then
      log_fatal(*) "MPR: n_horizons not set for domain ", n2s(id(1)), ". Error: ", trim(errmsg)
      error stop 1
    end if
    if (self%config%n_horizons(id(1)) < 1_i4) then
      log_fatal(*) "MPR: n_horizons must be >= 1 for domain ", n2s(id(1)), "."
      error stop 1
    end if

    status = self%config%is_set("lai_time_step", idx=id, errmsg=errmsg)
    if (status /= NML_OK) then
      log_fatal(*) "MPR: lai_time_step not set for domain ", n2s(id(1)), ". Error: ", trim(errmsg)
      error stop 1
    end if

    select case (self%config%soil_db_mode(id(1)))
      case (0_i4)
        required_depths = max(0_i4, self%config%n_horizons(id(1)) - 1_i4)
      case (1_i4)
        required_depths = self%config%n_horizons(id(1))
      case default
        log_fatal(*) "MPR: unsupported soil_db_mode=", n2s(self%config%soil_db_mode(id(1))), "."
        error stop 1
    end select
    do i = 1_i4, required_depths
      depth_idx = [i, id(1)]
      status = self%config%is_set("soil_depth", idx=depth_idx, errmsg=errmsg)
      if (status /= NML_OK) then
        log_fatal(*) "MPR: soil_depth(", n2s(i), ",", n2s(id(1)), ") not set. Error: ", trim(errmsg)
        error stop 1
      end if
    end do

    status = self%config%is_set("soil_lut_path", idx=id, errmsg=errmsg)
    if (status /= NML_OK) then
      log_fatal(*) "MPR: soil_lut_path not set for domain ", n2s(id(1)), ". Error: ", trim(errmsg)
      error stop 1
    end if
    self%soil_lut_path = self%exchange%get_path(self%config%soil_lut_path(id(1)))

    status = self%config%is_set("geo_lut_path", idx=id, errmsg=errmsg)
    if (status /= NML_OK) then
      log_fatal(*) "MPR: geo_lut_path not set for domain ", n2s(id(1)), ". Error: ", trim(errmsg)
      error stop 1
    end if
    self%geo_lut_path = self%exchange%get_path(self%config%geo_lut_path(id(1)))

    if (self%config%lai_time_step(id(1)) == 0_i4) then
      status = self%config%is_set("lai_lut_path", idx=id, errmsg=errmsg)
      if (status /= NML_OK) then
        log_fatal(*) "MPR: lai_lut_path not set for domain ", n2s(id(1)), &
          " while lai_time_step=0. Error: ", trim(errmsg)
        error stop 1
      end if
      self%lai_lut_path = self%exchange%get_path(self%config%lai_lut_path(id(1)))
      if (allocated(self%lai_path)) deallocate(self%lai_path)
    else
      status = self%config%is_set("lai_path", idx=id, errmsg=errmsg)
      if (status /= NML_OK) then
        log_fatal(*) "MPR: lai_path not set for domain ", n2s(id(1)), &
          " while lai_time_step=", n2s(self%config%lai_time_step(id(1))), ". Error: ", trim(errmsg)
        error stop 1
      end if
      self%lai_path = self%exchange%get_path(self%config%lai_path(id(1)))
      if (allocated(self%lai_lut_path)) deallocate(self%lai_lut_path)
    end if
  end subroutine mpr_configure

  !> \brief Connect the MPR process container with other components.
  subroutine mpr_connect(self)
    class(mpr_t), intent(inout), target :: self
    integer(i4) :: id(1)
    integer(i4) :: soil_layers
    log_info(*) "Connect MPR"

    id(1) = self%exchange%domain

    ! declare MPR prerequisites in the exchange contract
    self%exchange%slope%required = .true.
    self%exchange%aspect%required = .true.
    self%exchange%soil_id%required = .true.
    self%exchange%geo_unit%required = .true.
    self%exchange%gridded_lai%required = self%config%lai_time_step(id(1)) /= 0_i4

    if (.not.self%exchange%slope%provided) then
      log_fatal(*) "MPR: slope not provided (check input settings)."
      error stop 1
    end if
    if (.not.associated(self%exchange%slope%data)) then
      log_fatal(*) "MPR: slope marked as provided but data is not connected."
      error stop 1
    end if

    if (.not.self%exchange%aspect%provided) then
      log_fatal(*) "MPR: aspect not provided (check input settings)."
      error stop 1
    end if
    if (.not.associated(self%exchange%aspect%data)) then
      log_fatal(*) "MPR: aspect marked as provided but data is not connected."
      error stop 1
    end if

    if (.not.self%exchange%geo_unit%provided) then
      log_fatal(*) "MPR: geo_unit not provided (check geo_class input settings)."
      error stop 1
    end if
    if (.not.associated(self%exchange%geo_unit%data)) then
      log_fatal(*) "MPR: geo_unit marked as provided but data is not connected."
      error stop 1
    end if

    if (.not.self%exchange%soil_id%provided) then
      log_fatal(*) "MPR: soil_id not provided (check soil_class/soil_horizon_class input settings)."
      error stop 1
    end if
    if (.not.associated(self%exchange%soil_id%data)) then
      log_fatal(*) "MPR: soil_id marked as provided but data is not connected."
      error stop 1
    end if

    soil_layers = size(self%exchange%soil_id%data, 2)
    select case (self%config%soil_db_mode(id(1)))
      case (0_i4)
        if (soil_layers /= 1_i4) then
          log_fatal(*) "MPR: soil_db_mode=0 expects a single soil class layer, but got ", n2s(soil_layers), "."
          error stop 1
        end if
      case (1_i4)
        if (self%config%n_horizons(id(1)) < 1_i4) then
          log_fatal(*) "MPR: n_horizons must be >= 1 for soil_db_mode=1."
          error stop 1
        end if
        if (soil_layers < self%config%n_horizons(id(1))) then
          log_fatal(*) "MPR: soil horizon input provides ", n2s(soil_layers), " layers, but n_horizons=", &
            n2s(self%config%n_horizons(id(1))), "."
          error stop 1
        end if
      case default
        log_fatal(*) "MPR: unsupported soil_db_mode=", n2s(self%config%soil_db_mode(id(1))), "."
        error stop 1
    end select

    if (self%exchange%gridded_lai%required) then
      if (.not.self%exchange%gridded_lai%provided) then
        log_fatal(*) "MPR: gridded_lai not provided, but required for lai_time_step=", n2s(self%config%lai_time_step(id(1))), "."
        error stop 1
      end if
      if (.not.associated(self%exchange%gridded_lai%data)) then
        log_fatal(*) "MPR: gridded_lai marked as provided but data is not connected."
        error stop 1
      end if
      if (.not.allocated(self%lai_path)) then
        log_fatal(*) "MPR: internal error, lai_path not resolved in configure."
        error stop 1
      end if
    else if (.not.allocated(self%lai_lut_path)) then
      log_fatal(*) "MPR: internal error, lai_lut_path not resolved in configure."
      error stop 1
    end if

    if (.not.allocated(self%soil_lut_path)) then
      log_fatal(*) "MPR: internal error, soil_lut_path not resolved in configure."
      error stop 1
    end if
    if (.not.allocated(self%geo_lut_path)) then
      log_fatal(*) "MPR: internal error, geo_lut_path not resolved in configure."
      error stop 1
    end if

    if (.not.associated(self%exchange%geo_class_def)) allocate(self%exchange%geo_class_def)
    call self%exchange%geo_class_def%reset()
    log_info(*) "MPR: read geology LUT: ", self%geo_lut_path
    call read_geoformation_lut( &
      filename=trim(self%geo_lut_path), nGeo=self%exchange%geo_class_def%nGeo, &
      geo_unit=self%exchange%geo_class_def%geo_unit, geo_karstic=self%exchange%geo_class_def%geo_karstic)
    if (self%exchange%geo_class_def%nGeo < 1_i4) then
      log_fatal(*) "MPR: geology LUT contains no classes: ", self%geo_lut_path
      error stop 1
    end if
  end subroutine mpr_connect

  !> \brief Initialize the MPR process container for the simulation.
  subroutine mpr_initialize(self)
    class(mpr_t), intent(inout), target :: self
    log_info(*) "Initialize MPR"
  end subroutine mpr_initialize

  !> \brief Update the MPR process container for the current time step.
  subroutine mpr_update(self)
    class(mpr_t), intent(inout), target :: self
    log_trace(*) "Update MPR"
  end subroutine mpr_update

  !> \brief Finalize the MPR process container after the simulation.
  subroutine mpr_finalize(self)
    class(mpr_t), intent(inout), target :: self
    log_info(*) "Finalize MPR"
  end subroutine mpr_finalize
end module mo_mpr_container
