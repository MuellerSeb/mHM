!> \file    mo_mhm_container.f90
!> \brief   \copybrief mo_mhm_container
!> \details \copydetails mo_mhm_container

!> \brief   Module for a mHM process container.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Aug 2025
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_exchange
module mo_mhm_container

  !> \class   mhm_t
  !> \brief   Class for a single mHM process container.
  type, public :: mhm_t
    logical :: active = .false. !< flag to activate the mHM process container
  contains
    procedure :: init => mhm_init
  end type mhm_t

  !> \class   mhm_config_t
  !> \brief   Class for a single mHM process container.
  type, public :: mhm_config_t
    logical :: active = .false. !< flag to activate the mHM process container
  end type mhm_config_t

contains

  !> \brief Initialize the mHM process container.
  subroutine mhm_init(self, config)
    class(mhm_t), intent(inout) :: self
    type(mhm_config_t), intent(in) :: config !< initialization config for mHM
    self%active = config%active
  end subroutine mhm_init

end module mo_mhm_container
