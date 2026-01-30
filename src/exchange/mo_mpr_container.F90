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
  use mo_message, only: message, error_message
  use nml_config_mpr, only: nml_config_mpr_t, NML_OK

  !> \class   mpr_t
  !> \brief   Class for a single MPR process container.
  type, public :: mpr_t
    type(nml_config_mpr_t) :: config !< configuration of the MPR process container
    type(exchange_t), pointer :: exchange => null() !< exchange container of the domain
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
  end subroutine mpr_configure

  !> \brief Connect the MPR process container with other components.
  subroutine mpr_connect(self)
    class(mpr_t), intent(inout), target :: self
    log_info(*) "Connect MPR"
  end subroutine mpr_connect

  !> \brief Initialize the MPR process container for the simulation.
  subroutine mpr_initialize(self)
    class(mpr_t), intent(inout), target :: self
    log_info(*) "Initialize MPR"
  end subroutine mpr_initialize

  !> \brief Update the MPR process container for the current time step.
  subroutine mpr_update(self)
    class(mpr_t), intent(inout), target :: self
    log_debug(*) "Update MPR"
  end subroutine mpr_update

  !> \brief Finalize the MPR process container after the simulation.
  subroutine mpr_finalize(self)
    class(mpr_t), intent(inout), target :: self
    log_info(*) "Finalize MPR"
  end subroutine mpr_finalize
end module mo_mpr_container
