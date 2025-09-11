!> \file    mo_meteo_container.f90
!> \brief   \copybrief mo_meteo_container
!> \details \copydetails mo_meteo_container

!> \brief   Module for a meteo process container.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Aug 2025
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_exchange
module mo_meteo_container
  use mo_kind, only: i4
  use mo_exchange_type, only: exchange_t
  use mo_message, only: message, error_message

  !> \class   meteo_config_t
  !> \brief   Class for a single Meteorology process container.
  type, public :: meteo_config_t
    logical :: active = .false. !< flag to activate the Meteorology process container
    integer(i4) :: domain !< domain number to read correct configuration
  contains
    procedure :: read => meteo_config_read
  end type meteo_config_t

  !> \class   meteo_t
  !> \brief   Class for a single Meteorology process container.
  type, public :: meteo_t
    type(meteo_config_t) :: config !< configuration of the Meteorology process container
  contains
    procedure :: init => meteo_init
    procedure :: connect => meteo_connect
    procedure :: prepare => meteo_prepare
    procedure :: update => meteo_update
  end type meteo_t

contains

  !> \brief Initialize the Meteorology process container.
  subroutine meteo_init(self, config)
    class(meteo_t), intent(inout) :: self
    type(meteo_config_t), intent(in) :: config !< initialization config for Meteorology
    call message(" ... init meteo")
    self%config = config
  end subroutine meteo_init

  !> \brief Initialize the meteo configuration.
  subroutine meteo_config_read(self, file, domain)
    class(meteo_config_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelists
    integer(i4), intent(in) :: domain !< domain number to read correct configuration
    call message(" ... config meteo: ", file)
    self%active = .true.
    self%domain = domain
  end subroutine meteo_config_read

  subroutine meteo_connect(this, exchange)
    class(meteo_t), intent(inout) :: this
    type(exchange_t), intent(inout) :: exchange
    call message(" ... connecting meteo: ", exchange%time%str())
  end subroutine meteo_connect

  subroutine meteo_prepare(this, exchange)
    class(meteo_t), intent(inout) :: this
    type(exchange_t), intent(inout) :: exchange
    call message(" ... preparing meteo: ", exchange%time%str())
  end subroutine meteo_prepare

  subroutine meteo_update(this, exchange)
    class(meteo_t), intent(inout) :: this
    type(exchange_t), intent(in) :: exchange
    call message(" ... updating meteo: ", exchange%time%str())
  end subroutine meteo_update

end module mo_meteo_container
