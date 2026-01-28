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
  use nml_config_meteo, only: nml_config_meteo_t, NML_OK

  !> \class   meteo_t
  !> \brief   Class for a single Meteorology process container.
  type, public :: meteo_t
    type(nml_config_meteo_t) :: config !< configuration of the Meteorology process container
    type(exchange_t), pointer :: exchange => null() !< exchange container of the domain
  contains
    procedure :: configure => meteo_configure
    procedure :: connect => meteo_connect
    procedure :: initialize => meteo_initialize
    procedure :: update => meteo_update
  end type meteo_t

contains

  !> \brief Configure the Meteorology process container.
  subroutine meteo_configure(self, file)
    class(meteo_t), intent(inout) :: self
    character(*), intent(in), optional :: file !< file containing the namelists
    character(1024) :: errmsg
    character(:), allocatable :: path
    integer :: status
    call message(" ... configure meteo")
    if (present(file)) then
      path = self%exchange%get_path(file) ! get absolute path relative to cwd
      call message(" ... read meteo config: ", path)
      status = self%config%from_file(file=path, errmsg=errmsg)
      if (status /= NML_OK) call error_message("Error reading meteo config from: ", path, ", with error: ", trim(errmsg))
    end if
    if (.not.self%config%is_configured) call error_message("Meteo configuration not set.")
    status = self%config%is_valid(errmsg=errmsg)
    if (status /= NML_OK) call error_message("Meteo config not valid. Error: ", trim(errmsg))
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
    call message(" ... updating meteo: ", self%exchange%time%str())
  end subroutine meteo_update

end module mo_meteo_container
