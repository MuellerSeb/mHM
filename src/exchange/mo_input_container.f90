!> \file    mo_input_container.f90
!> \brief   \copybrief mo_input_container
!> \details \copydetails mo_input_container

!> \brief   Module for an input container.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Aug 2025
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_exchange
module mo_input_container
  use mo_kind, only: i4
  use mo_exchange_type, only: exchange_t
  use mo_message, only: message, error_message

  !> \class   input_config_t
  !> \brief   Class for a single Input container.
  type, public :: input_config_t
    logical :: active = .false. !< flag to activate the Input container
    integer(i4) :: domain !< domain number to read correct configuration
  contains
    procedure :: read => input_config_read
  end type input_config_t

  !> \class   input_t
  !> \brief   Class for a single Input container.
  type, public :: input_t
    type(input_config_t) :: config !< configuration of the Input container
  contains
    procedure :: init => input_init
    procedure :: connect => input_connect
    procedure :: prepare => input_prepare
    procedure :: update => input_update
  end type input_t

contains

  !> \brief Initialize the Input container.
  subroutine input_init(self, config)
    class(input_t), intent(inout) :: self
    type(input_config_t), intent(in) :: config !< initialization config for Input
    call message(" ... init input")
    self%config = config
  end subroutine input_init

  !> \brief Initialize the input configuration.
  subroutine input_config_read(self, file, domain)
    class(input_config_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelists
    integer(i4), intent(in) :: domain !< domain number to read correct configuration
    call message(" ... config input: ", file)
    self%active = .true.
    self%domain = domain
  end subroutine input_config_read

  subroutine input_connect(this, exchange)
    class(input_t), intent(inout) :: this
    type(exchange_t), intent(inout) :: exchange
    call message(" ... connecting input: ", exchange%time%str())
  end subroutine input_connect

  subroutine input_prepare(this, exchange)
    class(input_t), intent(inout) :: this
    type(exchange_t), intent(inout) :: exchange
    call message(" ... preparing input: ", exchange%time%str())
  end subroutine input_prepare

  subroutine input_update(this, exchange)
    class(input_t), intent(inout) :: this
    type(exchange_t), intent(in) :: exchange
    call message(" ... updating input: ", exchange%time%str())
  end subroutine input_update

end module mo_input_container
