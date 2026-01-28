!> \file    mo_mpr_container.f90
!> \brief   \copybrief mo_mpr_container
!> \details \copydetails mo_mpr_container

!> \brief   Module for a mpr process container.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Aug 2025
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_exchange
module mo_mpr_container
  use mo_kind, only: i4
  use mo_exchange_type, only: exchange_t
  use mo_message, only: message, error_message
  use nml_config_mpr, only: nml_config_mpr_t, NML_OK

  !> \class   mpr_t
  !> \brief   Class for a single MPR process container.
  type, public :: mpr_t
    type(nml_config_mpr_t) :: config !< configuration of the MPR process container
    type(exchange_t), pointer :: exchange => null() !< exchange container of the domain
  contains
    procedure :: configure => mpr_configure
    procedure :: connect => mpr_connect
    procedure :: initialize => mpr_initialize
    procedure :: update => mpr_update
  end type mpr_t

contains

  !> \brief Configure the MPR process container.
  subroutine mpr_configure(self, file)
    class(mpr_t), intent(inout) :: self
    character(*), intent(in), optional :: file !< file containing the namelists
    character(1024) :: errmsg
    character(:), allocatable :: path
    integer :: status
    call message(" ... configure mpr")
    if (present(file)) then
      path = self%exchange%get_path(file) ! get absolute path relative to cwd
      call message(" ... read MPR config: ", path)
      status = self%config%from_file(file=path, errmsg=errmsg)
      if (status /= NML_OK) call error_message("Error reading MPR config from: ", path, ", with error: ", trim(errmsg))
    end if
    if (.not.self%config%is_configured) call error_message("MPR configuration not set.")
    status = self%config%is_valid(errmsg=errmsg)
    if (status /= NML_OK) call error_message("MPR config not valid. Error: ", trim(errmsg))
  end subroutine mpr_configure

  subroutine mpr_connect(self)
    class(mpr_t), intent(inout) :: self
    call message(" ... connecting mpr: ", self%exchange%time%str())
  end subroutine mpr_connect

  subroutine mpr_initialize(self)
    class(mpr_t), intent(inout) :: self
    call message(" ... initialize mpr: ", self%exchange%time%str())
  end subroutine mpr_initialize

  subroutine mpr_update(self)
    class(mpr_t), intent(inout) :: self
    ! call message(" ... updating mpr: ", self%exchange%time%str())
  end subroutine mpr_update

end module mo_mpr_container
