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
  contains
    procedure :: read => mrm_config_read
  end type mrm_config_t

  !> \class   mrm_t
  !> \brief   Class for a single mRM process container.
  type, public :: mrm_t
    type(mrm_config_t) :: config !< configuration of the mRM process container
    type(exchange_t), pointer :: exchange => null() !< exchange container of the domain
  contains
    procedure :: configure => mrm_configure
    procedure :: connect => mrm_connect
    procedure :: initialize => mrm_initialize
    procedure :: update => mrm_update
  end type mrm_t

contains

  !> \brief Configure the mRM process container.
  subroutine mrm_configure(self, config, exchange)
    class(mrm_t), intent(inout) :: self
    type(mrm_config_t), intent(in) :: config !< initialization config for mRM
    type(exchange_t), intent(in), pointer :: exchange !< exchange container of the domain
    call message(" ... configure mrm")
    self%config = config
    self%exchange => exchange
  end subroutine mrm_configure

  !> \brief Initialize the mrm configuration.
  subroutine mrm_config_read(self, file, output_file)
    class(mrm_config_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelists
    character(*), intent(in) :: output_file !< file containing the output namelist
    call message(" ... read config mrm: ", file, ", ", output_file)
    self%active = .true.
  end subroutine mrm_config_read

  subroutine mrm_connect(self)
    class(mrm_t), intent(inout) :: self
    call message(" ... connecting mrm: ", self%exchange%time%str())
  end subroutine mrm_connect

  subroutine mrm_initialize(self)
    class(mrm_t), intent(inout) :: self
    call message(" ... initialize mrm: ", self%exchange%time%str())
  end subroutine mrm_initialize

  subroutine mrm_update(self)
    class(mrm_t), intent(inout) :: self
    call message(" ... updating mrm: ", self%exchange%time%str())
  end subroutine mrm_update

end module mo_mrm_container
