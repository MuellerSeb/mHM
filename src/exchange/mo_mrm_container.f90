!> \file    mo_mrm_container.f90
!> \brief   \copybrief mo_mrm_container
!> \details \copydetails mo_mrm_container

!> \brief   Module for a mHM process container.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Aug 2025
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_exchange
module mo_mrm_container
  use mo_kind, only: i4
  use mo_exchange_type, only: exchange_t
  use mo_message, only: message, error_message

  !> \class   mrm_config_t
  !> \brief   Configuration for a single mRM process container.
  type, public :: mrm_config_t
    logical :: active = .false. !< flag to activate the mRM process container
    integer(i4) :: domain !< domain number to read correct configuration
  contains
    procedure :: read => mrm_config_read
  end type mrm_config_t

  !> \class   mrm_t
  !> \brief   Class for a single mRM process container.
  type, public :: mrm_t
    type(mrm_config_t) :: config !< configuration of the mRM process container
  contains
    procedure :: init => mrm_init
    procedure :: connect => mrm_connect
    procedure :: prepare => mrm_prepare
    procedure :: update => mrm_update
  end type mrm_t

contains

  !> \brief Initialize the mRM process container.
  subroutine mrm_init(self, config)
    class(mrm_t), intent(inout) :: self
    type(mrm_config_t), intent(in) :: config !< initialization config for mRM
    call message(" ... init mrm")
    self%config = config
  end subroutine mrm_init

  !> \brief Initialize the mrm configuration.
  subroutine mrm_config_read(self, file, output_file, domain)
    class(mrm_config_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelists
    character(*), intent(in) :: output_file !< file containing the output namelist
    integer(i4), intent(in) :: domain !< domain number to read correct configuration
    call message(" ... config mrm: ", file, ", ", output_file)
    self%active = .true.
    self%domain = domain
  end subroutine mrm_config_read

  subroutine mrm_connect(this, exchange)
    class(mrm_t), intent(inout) :: this
    type(exchange_t), intent(inout) :: exchange
    call message(" ... connecting mrm: ", exchange%time%str())
  end subroutine mrm_connect

  subroutine mrm_prepare(this, exchange)
    class(mrm_t), intent(inout) :: this
    type(exchange_t), intent(inout) :: exchange
    call message(" ... preparing mrm: ", exchange%time%str())
  end subroutine mrm_prepare

  subroutine mrm_update(this, exchange)
    class(mrm_t), intent(inout) :: this
    type(exchange_t), intent(in) :: exchange
    call message(" ... updating mrm: ", exchange%time%str())
  end subroutine mrm_update

end module mo_mrm_container
