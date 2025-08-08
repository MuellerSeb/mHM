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

  !> \class   input_config_t
  !> \brief   Class for a single Input container.
  type, public :: input_config_t
    logical :: active = .false. !< flag to activate the Input container
  end type input_config_t

  !> \class   input_t
  !> \brief   Class for a single Input container.
  type, public :: input_t
    type(input_config_t) :: config !< configuration of the Input container
  contains
    procedure :: init => input_init
  end type input_t

contains

  !> \brief Initialize the Input container.
  subroutine input_init(self, config)
    class(input_t), intent(inout) :: self
    type(input_config_t), intent(in) :: config !< initialization config for Input
    self%config = config
  end subroutine input_init

end module mo_input_container
