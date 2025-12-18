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

  use mo_kind, only: i4, dp
  use mo_list, only: list
  use mo_exchange_type, only: exchange_t
  use mo_message, only: message, error_message
  use mo_datetime, only: datetime, timedelta, HOUR_SECONDS, DAY_HOURS, one_hour, one_day 
  use mo_grid, only: grid_t
  use mo_grid_io, only: var, input_dataset, end_timestamp
  use mo_string_utils, only: n2s => num2str
  use mo_namelists, only: nml_directories_mhm_t, nml_coupling_t

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
    ! variables
    character(1024)  :: chunk            ! reading chunk size: off (default - 0), monthly (1), yearly (2), once (not sure what this should be)
    character(1024)  :: runoff_file
    character(1024)  :: runoff_vname
!    type(nml_directories_mhm_t) :: directories_mhm !< directories for mHM input files
!    type(nml_coupling_t) :: coupling !< coupling configuration
  contains
    procedure :: read => input_config_read
  end type input_config_t

  !> \class   input_t
  !> \brief   Class for a single Input container.
  type, public :: input_t
    type(input_config_t) :: config !< configuration of the Input container
    type(exchange_t), pointer :: exchange => null() !< exchange container of the domain

    ! chunk bounds for reading
    type(datetime) :: chunk_time_start, chunk_time_end
    ! provide runoff
    integer(i4)           :: runoff_input_step
    integer(i4)           :: chunk_offset
    real(dp), allocatable :: runoff(:) ! current step
    real(dp), allocatable :: runoff_chunk(:,:)
    type(grid_t)          :: level1
    type(input_dataset)   :: input_runoff
    type(input_list)      :: datasets !< list of input datasets

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

  !> \brief Initialize the input configuration.
!  subroutine input_config_read(self, file)
!    class(input_config_t), intent(inout) :: self
!    character(*), intent(in) :: file !< file containing the namelists
!    call message(" ... read config input: ", file)
!    self%active = .true.
!    call self%directories_mhm%read(file)
!   call self%coupling%read(file)
!  end subroutine input_config_read

  !> \brief Configure the Input container.
  subroutine input_configure(self, config, exchange)
    class(input_t), target, intent(inout) :: self
    type(input_config_t), intent(in) :: config !< initialization config for Input
    type(exchange_t), intent(in), pointer :: exchange !< exchange container of the domain
    call message(" ... configure input")
    self%config = config
    self%exchange => exchange
  end subroutine input_configure

  !> \brief Initialize the input configuration.
  subroutine input_config_read(self, file)
    use mo_nml, only: position_nml ! , close_nml
    use mo_namelists, only: open_new_nml, close_nml
    
    ! input/output variables
    class(input_config_t), target, intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelists
    
    ! local variables
    integer(i4)     :: unit
    character(1024) :: chunk            ! reading chunk size: off (default - 0), monthly (1), yearly (2), once (not sure what this should be)
    character(1024) :: runoff_file
    character(1024) :: runoff_vname
    
    namelist/runoff_input/chunk, runoff_file, runoff_vname

    call message(" ... read config input: ", file)
    self%active = .true.

    ! read mRM input configuration
  call open_new_nml(file, unit)
  call position_nml('runoff_input', unit)
  read(unit, nml=runoff_input)
  call close_nml(unit)

  self%chunk = chunk
  self%runoff_file = runoff_file
  self%runoff_vname = runoff_vname

  end subroutine input_config_read

  subroutine input_connect(self)
    ! input/output variables
    class(input_t), target, intent(inout) :: self
    ! local variables
    logical              :: chunking
    integer(i4)          :: input_step
    character(1024)      :: file
    character(1024)      :: vname
    type(timedelta)      :: model_step
    call message(" ... connecting input: ", self%exchange%time%str())
    ! read values
    ! <- runoff file if required
    file = self%config%runoff_file
    vname = self%config%runoff_vname
    call message("open runoff file: ", file)
    call self%input_runoff%init(path=file, grid=self%level1, vars=[var(name=vname, static=.false.)], &
      timestamp=end_timestamp, grid_init_var=vname, tol=1.e-4_dp)
    ! model time config
    if (self%input_runoff%static) call error_message("runoff file is static.")
    if (self%input_runoff%timestep > 0_i4) model_step = one_hour() * self%input_runoff%timestep
    if (self%input_runoff%timestep == -1_i4) model_step = one_day()
    if (self%input_runoff%timestep < -1_i4) call error_message("runoff file needs to have daily or hourly values.")
    self%runoff_input_step = model_step%days * DAY_HOURS + model_step%seconds / HOUR_SECONDS
    call message(" ... input step [h]: ", n2s(input_step))
    ! call self%input_runoff%read_chunk_by_ids('Q', arr, 1_i4, 10_i4)
    ! print *, shape(arr)

    call message("Prepare input reading")
    chunking = self%config%chunk /= "off"
    if (chunking) then
      self%chunk_time_start = self%exchange%start_time
      select case(self%config%chunk)
        case("monthly")
          call message(" ... monthly chunks")
          self%chunk_time_end = self%chunk_time_start%next_new_month()
        case("yearly")
          call message(" ... yearly chunks")
          self%chunk_time_end = self%chunk_time_start%next_new_year()
        case("once")
          call message(" ... load all input into memory")
          self%chunk_time_end = self%exchange%end_time
        case default
          call error_message("Chunk not valid: ", self%config%chunk)
      end select
      call message(" ... read chunk: ", self%chunk_time_start%str(), " to ", self%chunk_time_end%str())
      call self%input_runoff%read_chunk(trim(vname), self%runoff_chunk, self%chunk_time_start, self%chunk_time_end)
    else
      call message(" ... read each input separately")
      ! allocate pointer
      allocate(self%runoff_chunk(self%level1%ncells, 1), source=0.0_dp)
    end if

    ! exchange points to runoff
    self%exchange%runoff_total%data => self%runoff_chunk(:, 1)
    self%exchange%runoff_total%stepping = self%runoff_input_step
    self%exchange%level1 => self%level1

  end subroutine input_connect

  subroutine input_initialize(self)
    class(input_t), target, intent(inout) :: self
    call message(" ... initialize input: ", self%exchange%time%str())

    print *, 'reset chunking'
      ! if (start_time == start_time_frame) then
      !   chunk_offset = 0_i4
      ! else
      !   chunk_offset = input%time_index(start_time)
      ! end if

  end subroutine input_initialize

  subroutine input_update(self)
    class(input_t), target, intent(inout) :: self
    call message(" ... updating input: ", self%exchange%time%str())

    call input_update_mrm(self)

  end subroutine input_update

  ! updates input for mRM
  subroutine input_update_mrm(self)
    class(input_t), target, intent(inout) :: self
    integer(i4) :: slice

    call message(" ... updating reading of runoff")

    if (self%config%chunk /= 'off') then
      ! update chunk
      if (self%exchange%time > self%chunk_time_end) then
        self%chunk_time_start = self%chunk_time_end
        select case(self%config%chunk)
          case("monthly")
            self%chunk_time_end = self%chunk_time_start%next_new_month()
          case("yearly")
            self%chunk_time_end = self%chunk_time_start%next_new_year()
        end select
        call message(" ... read new chunk: ", self%chunk_time_start%str(), " to ", self%chunk_time_end%str())
        call self%input_runoff%read_chunk(trim(self%config%runoff_vname), self%runoff_chunk, self%chunk_time_start, self%chunk_time_end)
        self%chunk_offset = self%input_runoff%time_index(self%chunk_time_start) + 2_i4
        self%exchange%runoff_total%data => self%runoff_chunk(:, self%chunk_offset - self%input_runoff%time_index(self%chunk_time_start))
      end if

      ! update slice
      if (self%exchange%time > self%input_runoff%times(self%chunk_offset)) then
        ! print *, 'exchange time: ', self%exchange%time
        ! print *, 'time step:     ', self%input_runoff%times(self%chunk_offset)
        self%chunk_offset = self%chunk_offset + 1_i4
        ! print *, 'new slice: ', self%chunk_offset - self%input_runoff%time_index(self%chunk_time_start) 
        self%exchange%runoff_total%data => self%runoff_chunk(:, self%chunk_offset - self%input_runoff%time_index(self%chunk_time_start)) 
      end if
      ! print *, self%exchange%runoff_total%data(:)
    else
      call self%input_runoff%read(trim(self%config%runoff_vname), self%runoff_chunk(:, 1), self%exchange%time) ! read every time-step separately
      self%exchange%runoff_total%data => self%runoff_chunk(:, 1)  
    end if
  end subroutine input_update_mrm

end module mo_input_container
