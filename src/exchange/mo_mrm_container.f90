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

  !> \class   mrm_t
  !> \brief   Class for a single mRM process container.
  type, public :: mrm_t
    logical :: active = .false. !< flag to activate the mRM process container
  contains
    procedure :: init => mrm_init
  end type mrm_t

  !> \class   mrm_config_t
  !> \brief   Class for a single mRM process container.
  type, public :: mrm_config_t
    logical :: active = .false. !< flag to activate the mRM process container
  end type mrm_config_t

contains

  !> \brief Initialize the mRM process container.
  subroutine mrm_init(self, config)
    class(mrm_t), intent(inout) :: self
    type(mrm_config_t), intent(in) :: config !< initialization config for mRM
    self%active = config%active
  end subroutine mrm_init

end module mo_mrm_container
