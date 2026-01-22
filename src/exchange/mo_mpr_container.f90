!> \file    mo_mpr_container.f90
!> \brief   \copybrief mo_mpr_container
!> \details \copydetails mo_mpr_container

!> \brief   Module for a mpr process container.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Aug 2025
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_exchange
module mo_mpr_container
  use mo_kind, only: i4
  use mo_exchange_type, only: exchange_t
  use mo_message, only: message, error_message

  !> \class   mpr_config_t
  !> \brief   Configuration for a single MPR process container.
  type, public :: mpr_config_t
    logical :: active = .false. !< flag to activate the MPR process container
  contains
    procedure :: read => mpr_config_read
  end type mpr_config_t

  !> \class   mpr_t
  !> \brief   Class for a single MPR process container.
  type, public :: mpr_t
    type(mpr_config_t) :: config !< configuration of the MPR process container
    type(exchange_t), pointer :: exchange => null() !< exchange container of the domain
  contains
    procedure :: configure => mpr_configure
    procedure :: connect => mpr_connect
    procedure :: initialize => mpr_initialize
    procedure :: update => mpr_update
  end type mpr_t

contains

  !> \brief Configure the MPR process container.
  subroutine mpr_configure(self, config, exchange)
    class(mpr_t), intent(inout) :: self
    type(mpr_config_t), intent(in) :: config !< initialization config for MPR
    type(exchange_t), intent(in), pointer :: exchange !< exchange container of the domain
    call message(" ... configure mpr")
    self%config = config
    self%exchange => exchange
  end subroutine mpr_configure

  !> \brief Initialize the mpr configuration.
  subroutine mpr_config_read(self, file)
    class(mpr_config_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelists
    call message(" ... read config mpr: ", file)
    self%active = .true.
  end subroutine mpr_config_read

  subroutine mpr_connect(self)
    class(mpr_t), intent(inout) :: self
    call message(" ... connecting mpr: ", self%exchange%time%str())
  end subroutine mpr_connect

  subroutine mpr_initialize(self)
    class(mpr_t), intent(inout) :: self
    call message(" ... initialize mpr: ", self%exchange%time%str())
  end subroutine mpr_initialize

  subroutine mpr_update(self)
    class(mpr_t), intent(inout) :: self
    ! call message(" ... updating mpr: ", self%exchange%time%str())
  end subroutine mpr_update

end module mo_mpr_container
