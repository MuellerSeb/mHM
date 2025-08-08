!> \file    mo_meteo_container.f90
!> \brief   \copybrief mo_meteo_container
!> \details \copydetails mo_meteo_container

!> \brief   Module for a mHM process container.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Aug 2025
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_exchange
module mo_meteo_container

  !> \class   meteo_config_t
  !> \brief   Class for a single Meteorology process container.
  type, public :: meteo_config_t
    logical :: active = .false. !< flag to activate the Meteorology process container
  end type meteo_config_t

  !> \class   meteo_t
  !> \brief   Class for a single Meteorology process container.
  type, public :: meteo_t
    type(meteo_config_t) :: config !< configuration of the Meteorology process container
  contains
    procedure :: init => meteo_init
  end type meteo_t

contains

  !> \brief Initialize the Meteorology process container.
  subroutine meteo_init(self, config)
    class(meteo_t), intent(inout) :: self
    type(meteo_config_t), intent(in) :: config !< initialization config for Meteorology
    self%config = config
  end subroutine meteo_init

end module mo_meteo_container
