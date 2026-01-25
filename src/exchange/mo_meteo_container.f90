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
  contains
    procedure :: read => meteo_config_read
  end type meteo_config_t

  !> \class   meteo_t
  !> \brief   Class for a single Meteorology process container.
  type, public :: meteo_t
    type(meteo_config_t) :: config !< configuration of the Meteorology process container
    type(exchange_t), pointer :: exchange => null() !< exchange container of the domain
  contains
    procedure :: configure => meteo_configure
    procedure :: connect => meteo_connect
    procedure :: initialize => meteo_initialize
    procedure :: update => meteo_update
  end type meteo_t

contains

  !> \brief Initialize the meteo configuration.
  subroutine meteo_config_read(self, file)
    class(meteo_config_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelists
    call message(" ... read config meteo: ", file)
    self%active = .true.
  end subroutine meteo_config_read

  !> \brief Configure the Meteorology process container.
  subroutine meteo_configure(self, config)
    class(meteo_t), intent(inout) :: self
    type(meteo_config_t), intent(in) :: config !< initialization config for Meteorology
    call message(" ... configure meteo")
    self%config = config
  end subroutine meteo_configure

  subroutine meteo_connect(self)
    class(meteo_t), intent(inout) :: self
    call message(" ... connecting meteo: ", self%exchange%time%str())
  end subroutine meteo_connect

  subroutine meteo_initialize(self)
    class(meteo_t), intent(inout) :: self
    call message(" ... initialize meteo: ", self%exchange%time%str())
  end subroutine meteo_initialize

  subroutine meteo_update(self)
    class(meteo_t), intent(inout) :: self
    ! call message(" ... updating meteo: ", self%exchange%time%str())
  end subroutine meteo_update

end module mo_meteo_container
