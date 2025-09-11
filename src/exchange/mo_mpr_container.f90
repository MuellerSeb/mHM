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
    integer(i4) :: domain !< domain number to read correct configuration
  contains
    procedure :: read => mpr_config_read
  end type mpr_config_t

  !> \class   mpr_t
  !> \brief   Class for a single MPR process container.
  type, public :: mpr_t
    type(mpr_config_t) :: config !< configuration of the MPR process container
  contains
    procedure :: init => mpr_init
    procedure :: connect => mpr_connect
    procedure :: prepare => mpr_prepare
    procedure :: update => mpr_update
  end type mpr_t

contains

  !> \brief Initialize the MPR process container.
  subroutine mpr_init(self, config)
    class(mpr_t), intent(inout) :: self
    type(mpr_config_t), intent(in) :: config !< initialization config for MPR
    call message(" ... init mpr")
    self%config = config
  end subroutine mpr_init

  !> \brief Initialize the mpr configuration.
  subroutine mpr_config_read(self, file, domain)
    class(mpr_config_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelists
    integer(i4), intent(in) :: domain !< domain number to read correct configuration
    call message(" ... config mpr: ", file)
    self%active = .true.
    self%domain = domain
  end subroutine mpr_config_read

  subroutine mpr_connect(this, exchange)
    class(mpr_t), intent(inout) :: this
    type(exchange_t), intent(inout) :: exchange
    call message(" ... connecting mpr: ", exchange%time%str())
  end subroutine mpr_connect

  subroutine mpr_prepare(this, exchange)
    class(mpr_t), intent(inout) :: this
    type(exchange_t), intent(inout) :: exchange
    call message(" ... preparing mpr: ", exchange%time%str())
  end subroutine mpr_prepare

  subroutine mpr_update(this, exchange)
    class(mpr_t), intent(inout) :: this
    type(exchange_t), intent(in) :: exchange
    call message(" ... updating mpr: ", exchange%time%str())
  end subroutine mpr_update

end module mo_mpr_container
