!> \file    mo_mpr_container.f90
!> \brief   \copybrief mo_mpr_container
!> \details \copydetails mo_mpr_container

!> \brief   Module for a mHM process container.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Aug 2025
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_exchange
module mo_mpr_container

  !> \class   mpr_t
  !> \brief   Class for a single MPR process container.
  type, public :: mpr_t
    logical :: active = .false. !< flag to activate the MPR process container
  contains
    procedure :: init => mpr_init
  end type mpr_t

  !> \class   mpr_config_t
  !> \brief   Class for a single MPR process container.
  type, public :: mpr_config_t
    logical :: active = .false. !< flag to activate the MPR process container
  end type mpr_config_t

contains

  !> \brief Initialize the MPR process container.
  subroutine mpr_init(self, config)
    class(mpr_t), intent(inout) :: self
    type(mpr_config_t), intent(in) :: config !< initialization config for MPR
    self%active = config%active
  end subroutine mpr_init

end module mo_mpr_container
