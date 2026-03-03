!> \file    mo_meteo_container.f90
!> \copydoc mo_meteo_container

!> \brief   Module for a meteo process container.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Aug 2025
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_exchange
#include "logging.h"
module mo_meteo_container
  use mo_logging
  use mo_kind, only: i4
  use mo_exchange_type, only: exchange_t
  use mo_message, only: message, error_message
  use nml_config_meteo, only: nml_config_meteo_t, NML_OK

  !> \class   meteo_t
  !> \brief   Class for a single Meteorology process container.
  type, public :: meteo_t
    type(nml_config_meteo_t) :: config !< configuration of the Meteorology process container
    type(exchange_t), pointer :: exchange => null() !< exchange container of the domain
  contains
    procedure :: configure => meteo_configure
    procedure :: connect => meteo_connect
    procedure :: initialize => meteo_initialize
    procedure :: update => meteo_update
    procedure :: finalize => meteo_finalize
  end type meteo_t

contains

  !> \brief Configure the Meteorology process container.
  subroutine meteo_configure(self, file)
    class(meteo_t), intent(inout), target :: self
    character(*), intent(in), optional :: file !< file containing the namelists
    character(1024) :: errmsg
    character(:), allocatable :: path
    integer :: status
    log_info(*) "Configure meteo"
    if (present(file)) then
      path = self%exchange%get_path(file) ! get absolute path relative to cwd
      log_info(*) "Read meteo config: ", path
      status = self%config%from_file(file=path, errmsg=errmsg)
      if (status /= NML_OK) then
        log_fatal(*) "Error reading meteo config: ", trim(errmsg)
        error stop 1
      end if
    end if
    if (.not.self%config%is_configured) then
      log_fatal(*) "Meteo config not set."
      error stop 1
    end if
    status = self%config%is_valid(errmsg=errmsg)
    if (status /= NML_OK) then
      log_fatal(*) "Meteo config not valid: ", trim(errmsg)
      error stop 1
    end if
  end subroutine meteo_configure

  !> \brief Connect the Meteorology process container with other components.
  subroutine meteo_connect(self)
    class(meteo_t), intent(inout), target :: self
    log_info(*) "Connect meteo"
  end subroutine meteo_connect

  !> \brief Initialize the Meteorology process container for the simulation.
  subroutine meteo_initialize(self)
    class(meteo_t), intent(inout), target :: self
    log_info(*) "Initialize meteo"
  end subroutine meteo_initialize

  !> \brief Update the Meteorology process container for the current time step.
  subroutine meteo_update(self)
    class(meteo_t), intent(inout), target :: self
    log_trace(*) "Update meteo"
  end subroutine meteo_update

  !> \brief Finalize the Meteorology process container after the simulation.
  subroutine meteo_finalize(self)
    class(meteo_t), intent(inout), target :: self
    log_info(*) "Finalize meteo"
  end subroutine meteo_finalize

end module mo_meteo_container
