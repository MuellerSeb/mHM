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
  use mo_kind, only: i4
  use mo_list, only: list
  use mo_grid_io, only: input_dataset
  use mo_exchange_type, only: exchange_t
  use mo_message, only: message, error_message
  use mo_string_utils, only: n2s => num2str

  !> \class   input_list
  !> \brief   Class to hold a list of input containers with absolute paths as keys.
  type, extends(list) :: input_list
  contains
    procedure :: get_input => input_list_get
    procedure :: add_input => input_list_add
  end type

  !> \class   input_config_t
  !> \brief   Class for a single Input container.
  type, public :: input_config_t
    logical :: active = .false. !< flag to activate the Input container
    integer(i4) :: domain !< domain number to read correct configuration
  contains
    procedure :: read => input_config_read
  end type input_config_t

  !> \class   input_t
  !> \brief   Class for a single Input container.
  type, public :: input_t
    type(input_config_t) :: config !< configuration of the Input container
    type(exchange_t), pointer :: exchange => null() !< exchange container of the domain
    type(input_list) :: datasets !< list of input datasets
  contains
    procedure :: configure => input_configure
    procedure :: connect => input_connect
    procedure :: initialize => input_initialize
    procedure :: update => input_update
  end type input_t

contains

  !> \brief Get pointer to desired input dataset from input list.
  subroutine input_list_get(self, path, input)
    class(input_list), intent(in) :: self
    character(len=*), intent(in) :: path !< input path
    type(input_dataset), pointer, intent(out) :: input !< pointer to desired input
    class(*), pointer :: p
    call self%get(path, p)
    if (associated(p)) then
      select type (p)
        type is (input_dataset)
          input => p
        class default
          call error_message("Input '", path, "' not a input_dataset type.")
      end select
    else
      call error_message("Input '", path, "' not present.")
    end if
  end subroutine input_list_get

  !> \brief Add a new input dataset to the input list.
  subroutine input_list_add(self, path)
    class(input_list), intent(inout) :: self
    character(len=*), intent(in) :: path !< input path
    type(input_dataset) :: new_input
    call self%add_clone(path, new_input)
  end subroutine input_list_add

  !> \brief Configure the Input container.
  subroutine input_configure(self, config, exchange)
    class(input_t), intent(inout) :: self
    type(input_config_t), intent(in) :: config !< initialization config for Input
    type(exchange_t), intent(in), pointer :: exchange !< exchange container of the domain
    call message(" ... configure input")
    self%config = config
    self%exchange => exchange
  end subroutine input_configure

  !> \brief Initialize the input configuration.
  subroutine input_config_read(self, file)
    class(input_config_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelists
    call message(" ... read config input: ", file)
    self%active = .true.
  end subroutine input_config_read

  subroutine input_connect(self)
    class(input_t), intent(inout) :: self
    call message(" ... connecting input: ", self%exchange%time%str())
  end subroutine input_connect

  subroutine input_initialize(self)
    class(input_t), intent(inout) :: self
    call message(" ... initialize input: ", self%exchange%time%str())
  end subroutine input_initialize

  subroutine input_update(self)
    class(input_t), intent(inout) :: self
    call message(" ... updating input: ", self%exchange%time%str())
  end subroutine input_update

end module mo_input_container
