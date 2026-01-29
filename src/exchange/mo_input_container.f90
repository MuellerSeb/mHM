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
  use mo_grid_io, only: var, input_dataset, end_timestamp, start_timestamp, no_time, daily, monthly, yearly, varying
  use mo_string_utils, only: n2s => num2str
  use nml_config_input, only: nml_config_input_t
  use nml_config_coupling, only: nml_config_coupling_t
  use nml_helper, only: NML_OK

  !> \class   input_var_abc
  !> \brief   Abstract Base Class for a single input variable.
  type, abstract :: input_var_abc
    logical :: provided = .false. !< whether this input variable is provided
    logical :: coupled = .false. !< whether this variable is from a coupler
    logical :: static = .false. !< whether this variable is static in time
    character(len=:), allocatable :: path !< path to the input dataset
    character(len=:), allocatable :: name !< variable name in the dataset
    type(input_dataset) :: ds !< input netcdf dataset
    integer(i4) :: var_id !< variable ID in the dataset
    integer(i4) :: stepping = 0_i4 !< time-stepping in hours (0 - static, -1 - daily, -2 -monthly, -3 - yearly, >0 - hourly)
    integer(i4) :: offset !< offset for reading in chunked mode
    type(datetime) :: chunk_time_start !< start time of current chunk (time span start when coupled)
    type(datetime) :: chunk_time_end !< end time of current chunk (time span end when coupled)
    logical, allocatable :: mask(:,:) !< optional mask for this variable size(nx, ny) (used for coupling)
  contains
    procedure :: init => input_var_init
    procedure :: open_dataset => input_var_open_dataset
    procedure :: check_couple_status => input_var_check_couple_status
    procedure :: set_mask => input_var_set_mask
  end type input_var_abc

  !> \class   input_var_dp
  !> \brief   Class for a single double precision input variable.
  type, public, extends(input_var_abc) :: input_var_dp
    real(dp), allocatable :: cache(:,:) !< data array for this variable size(ncells, ntimes)
  contains
    procedure :: update => input_var_dp_update
    procedure :: read_static => input_var_dp_read_static
    procedure :: reset_time => input_var_dp_reset_time
    ! procedure :: set => input_var_dp_set
  end type input_var_dp

  !> \class   input_var_i4
  !> \brief   Class for a single integer input variable.
  type, public, extends(input_var_abc) :: input_var_i4
    integer(i4), allocatable :: cache(:,:) !< data array for this variable size(ncells, ntimes)
  contains
    procedure :: update => input_var_i4_update
    procedure :: read_static => input_var_i4_read_static
    procedure :: reset_time => input_var_i4_reset_time
    ! procedure :: set => input_var_i4_set
  end type input_var_i4

  !> \class   input_var2d_dp
  !> \brief   Class for a single 2D double precision input variable.
  type, public, extends(input_var_abc) :: input_var2d_dp
    real(dp), allocatable :: cache(:,:,:) !< data array for this variable size(ncells, nlayers, ntimes)
  contains
    procedure :: update => input_var2d_dp_update
    procedure :: read_static => input_var2d_dp_read_static
    procedure :: reset_time => input_var2d_dp_reset_time
    ! procedure :: set => input_var2d_dp_set
  end type input_var2d_dp

  !> \class   input_var2d_i4
  !> \brief   Class for a single 2D integer input variable.
  type, public, extends(input_var_abc) :: input_var2d_i4
    integer(i4), allocatable :: cache(:,:,:) !< data array for this variable size(ncells, nlayers, ntimes)
  contains
    procedure :: update => input_var2d_i4_update
    procedure :: read_static => input_var2d_i4_read_static
    procedure :: reset_time => input_var2d_i4_reset_time
    ! procedure :: set => input_var2d_i4_set
  end type input_var2d_i4

  !> \class   input_config_t
  !> \brief   Class for a single Input container.
  type, public :: input_config_t
    logical :: active = .true. !< flag to activate the Input container
    integer(i4) :: domain !< domain number to read correct configuration
   type(nml_config_input_t) :: input !< configuration for inputs from files
   type(nml_config_coupling_t) :: coupling !< configuration for coupling
  contains
    procedure :: read => input_config_read
  end type input_config_t

  !> \class   input_t
  !> \brief   Class for a single Input container.
  type, public :: input_t
    type(input_config_t) :: config !< configuration of the Input container
    type(exchange_t), pointer :: exchange => null() !< exchange container of the domain
    type(grid_t) :: tgt_level0 !< grid level 0 of the domain if given from input
    type(grid_t) :: tgt_level1 !< grid level 1 of the domain if given from input
    type(grid_t) :: tgt_level2 !< grid level 2 of the domain if given from input
    type(grid_t) :: tgt_level3 !< grid level 3 of the domain if given from input
    integer(i4) :: chunking !< chunking configuration (0 single read, -1 daily, -2 monthly, -3 yearly, >0 every n hours)
    integer(i4) :: time_stamp_location !< location of time-stamp variable in input datasets (0 start, 1 center, 2 end)
    ! level 0 inputs
    type(input_var_i4) :: morph_mask !< input variable for morph mask
    type(input_var_dp) :: dem !< input variable for digital elevation model
    type(input_var_dp) :: slope !< input variable for slope
    type(input_var_dp) :: aspect !< input variable for aspect
    type(input_var_i4) :: fdir !< input variable for flow direction
    type(input_var_i4) :: facc !< input variable for flow accumulation
    ! level 1 inputs
    type(input_var_i4) :: hydro_mask !< input variable for hydro mask
    type(input_var_dp) :: runoff !< input variable for runoff
  contains
    procedure :: configure => input_configure
    procedure :: connect => input_connect
    procedure :: initialize => input_initialize
    procedure :: update => input_update
    procedure :: finalize => input_finalize
  end type input_t

contains

  !> \brief Check whether the grid needs to be initialized in the exchange variable.
  logical function need_grid(tgt_grid, exchange_grid)
    type(grid_t), intent(in), target :: tgt_grid !< target grid in input
    type(grid_t), intent(inout), pointer :: exchange_grid !< grid pointer in exchange
    need_grid = .not.associated(exchange_grid)
    if (need_grid) exchange_grid => tgt_grid
  end function need_grid

  !> \brief Update the time frame for chunked reading.
  subroutine update_time_frame(chunk_time_start, chunk_time_end, chunking, end_time)
    type(datetime), intent(inout) :: chunk_time_start !< start time of current chunk
    type(datetime), intent(inout) :: chunk_time_end !< end time of current chunk
    integer(i4), intent(in) :: chunking !< chunking configuration (0 single read, -1 daily, -2 monthly, -3 yearly, >0 every n hours)
    type(datetime), intent(in) :: end_time !< end time of the simulation (for chunking limits)
    chunk_time_start = chunk_time_end
    select case(chunking)
      case(daily)
        chunk_time_end = chunk_time_start%next_new_day()
      case(monthly)
        chunk_time_end = chunk_time_start%next_new_month()
      case(yearly)
        chunk_time_end = chunk_time_start%next_new_year()
      case(0_i4) ! flag for reading all at once
        chunk_time_end = end_time ! read once until end time
      case default
        if (chunking < 1_i4) call error_message("Input: Chunk not valid: ", n2s(chunking))
        chunk_time_end = chunk_time_start + timedelta(hours=chunking)
      end select
    if (chunk_time_end > end_time) chunk_time_end = end_time
  end subroutine update_time_frame

  !> \brief Initialize the input variable.
  subroutine input_var_init(self, path, name, static, coupled)
    class(input_var_abc), intent(inout) :: self
    character(*), intent(in), optional :: path !< path to the input dataset
    character(*), intent(in), optional :: name !< variable name in the dataset
    logical, intent(in), optional :: static !< whether this variable is static in time
    logical, intent(in), optional :: coupled !< whether this variable is from a coupler
    self%provided = .true. ! mark as provided when initialized
    if (present(path)) self%path = trim(path)
    if (present(name)) self%name = trim(name)
    if (present(static)) self%static = static
    if (present(coupled)) self%coupled = coupled
  end subroutine input_var_init

  subroutine input_var_open_dataset(self, kind, timestamp, grid, init_grid)
    class(input_var_abc), intent(inout) :: self
    character(*), intent(in) :: kind !< kind of variable to create dataset for
    integer(i4), intent(in) :: timestamp !< time stamp for the dataset
    type(grid_t), intent(in), pointer :: grid !< grid for the variable
    logical, intent(in) :: init_grid !< whether to initialize the grid
    character(:), allocatable :: grid_init_var
    if (.not.self%provided) return
    if (init_grid) grid_init_var = self%name ! allocate variable name for grid initialization
    call self%ds%init(path=self%path, vars=[var(name=trim(self%name), kind=kind, static=self%static)], &
      timestamp=timestamp, grid=grid, grid_init_var=grid_init_var) ! if grid_init_var is un-allocated, it is "not present"
    self%var_id = self%ds%var_index(self%name)
    if (.not.self%static) self%stepping = self%ds%timestep
  end subroutine input_var_open_dataset

  !> \brief Check whether a coupled variable is properly initialized and within time bounds.
  function input_var_check_couple_status(self) result(is_coupled)
    class(input_var_abc), intent(inout) :: self
    logical :: is_coupled !< whether this variable is coupled and should not be updated
    is_coupled = self%coupled
    ! if (is_coupled) then
    !   if (self%static) then
    !     if (.not.allocated(self%cache)) call error_message("Coupled static input variable not initialized: ", trim(self%name))
    !   else if (time <= self%chunk_time_start .or. time > self%chunk_time_end) then
    !     call error_message("Coupled input variable time out of valid bounds: ", trim(self%name))
    !   end if
    ! end if
  end function input_var_check_couple_status

  !> \brief Set a mask for this input variable from a coupler.
  subroutine input_var_set_mask(self, mask)
    class(input_var_abc), intent(inout) :: self
    logical, intent(in) :: mask(:,:) !< mask to set for this variable
    self%mask = mask
  end subroutine input_var_set_mask

  !> \brief Update a single double precision input variable.
  subroutine input_var_dp_update(self, exchange_var, time, chunking, end_time)
    class(input_var_dp), intent(inout), target :: self
    real(dp), intent(inout), pointer :: exchange_var(:) !< exchange variable to update
    type(datetime), intent(in) :: time !< current model time
    integer(i4), intent(in) :: chunking !< chunking configuration (0 single read, -1 daily, -2 monthly, -3 yearly, >0 every n hours)
    type(datetime), intent(in) :: end_time !< end time of the simulation (for chunking limits)
    if (.not.self%provided) return
    if (self%check_couple_status()) return ! coupled variables are not updated here
    if (self%static) return ! static variables do not need to be updated
    ! check chunking
    if (chunking /= 1_i4) then
      ! update chunk
      if (time > self%chunk_time_end) then
        call update_time_frame(self%chunk_time_start, self%chunk_time_end, chunking, end_time)
        ! call message(" ... read new chunk: ", self%chunk_time_start%str(), " to ", self%chunk_time_end%str())
        nullify(exchange_var)
        if (allocated(self%cache)) deallocate(self%cache)
        call self%ds%read_chunk(self%var_id, self%cache, self%chunk_time_start, self%chunk_time_end)
        self%offset = self%ds%time_index(self%chunk_time_start)
      end if
      exchange_var => self%cache(:, self%ds%time_index(time) - self%offset)
    else
      if (.not.allocated(self%cache)) allocate(self%cache(self%ds%grid%ncells, 1))
      if (self%stepping == 1_i4) then
        call self%ds%read(self%var_id, self%cache(:, 1), time)
      else
        ! if this variable stepping is not hourly, read only when time exceeds current time frame
        if (time > self%chunk_time_end) then
          self%chunk_time_start = self%chunk_time_end
          self%chunk_time_end = self%ds%times(self%ds%time_index(time))
          call self%ds%read(self%var_id, self%cache(:, 1), time)
        end if
      end if
      exchange_var => self%cache(:, 1)
    end if
  end subroutine input_var_dp_update

  !> \brief Update a single integer input variable.
  subroutine input_var_i4_update(self, exchange_var, time, chunking, end_time)
    class(input_var_i4), intent(inout), target :: self
    integer(i4), intent(inout), pointer :: exchange_var(:) !< exchange variable to update
    type(datetime), intent(in) :: time !< current model time
    integer(i4), intent(in) :: chunking !< chunking configuration (0 single read, -1 daily, -2 monthly, -3 yearly, >0 every n hours)
    type(datetime), intent(in) :: end_time !< end time of the simulation (for chunking limits)
    if (.not.self%provided) return
    if (self%check_couple_status()) return ! coupled variables are not updated here
    if (self%static) return ! static variables do not need to be updated
    ! check chunking
    if (chunking /= 1_i4) then
      ! update chunk
      if (time > self%chunk_time_end) then
        call update_time_frame(self%chunk_time_start, self%chunk_time_end, chunking, end_time)
        ! call message(" ... read new chunk: ", self%chunk_time_start%str(), " to ", self%chunk_time_end%str())
        nullify(exchange_var)
        if (allocated(self%cache)) deallocate(self%cache)
        call self%ds%read_chunk(self%var_id, self%cache, self%chunk_time_start, self%chunk_time_end)
        self%offset = self%ds%time_index(self%chunk_time_start)
      end if
      exchange_var => self%cache(:, self%ds%time_index(time) - self%offset)
    else
      if (.not.allocated(self%cache)) allocate(self%cache(self%ds%grid%ncells, 1))
      if (self%stepping == 1_i4) then
        call self%ds%read(self%var_id, self%cache(:, 1), time)
      else
        ! if this variable stepping is not hourly, read only when time exceeds current time frame
        if (time > self%chunk_time_end) then
          self%chunk_time_start = self%chunk_time_end
          self%chunk_time_end = self%ds%times(self%ds%time_index(time))
          call self%ds%read(self%var_id, self%cache(:, 1), time)
        end if
      end if
      exchange_var => self%cache(:, 1)
    end if
  end subroutine input_var_i4_update

  !> \brief Update a single 2D double precision input variable.
  subroutine input_var2d_dp_update(self, exchange_var, time, chunking, end_time)
    class(input_var2d_dp), intent(inout), target :: self
    real(dp), intent(inout), pointer :: exchange_var(:,:) !< exchange variable to update
    type(datetime), intent(in) :: time !< current model time
    integer(i4), intent(in) :: chunking !< chunking configuration (0 single read, -1 daily, -2 monthly, -3 yearly, >0 every n hours)
    type(datetime), intent(in) :: end_time !< end time of the simulation (for chunking limits)
    if (.not.self%provided) return
    if (self%check_couple_status()) return ! coupled variables are not updated here
    if (self%static) return ! static variables do not need to be updated
    ! check chunking
    if (chunking /= 1_i4) then
      ! update chunk
      if (time > self%chunk_time_end) then
        call update_time_frame(self%chunk_time_start, self%chunk_time_end, chunking, end_time)
        ! call message(" ... read new chunk: ", self%chunk_time_start%str(), " to ", self%chunk_time_end%str())
        nullify(exchange_var)
        if (allocated(self%cache)) deallocate(self%cache)
        call self%ds%read_chunk_layered(self%var_id, self%cache, self%chunk_time_start, self%chunk_time_end)
        self%offset = self%ds%time_index(self%chunk_time_start)
      end if
      exchange_var => self%cache(:, :, self%ds%time_index(time) - self%offset)
    else
      if (.not.allocated(self%cache)) allocate(self%cache(self%ds%grid%ncells, self%ds%nlayers, 1))
      if (self%stepping == 1_i4) then
        call self%ds%read_layered(self%var_id, self%cache(:, :, 1), time)
      else
        ! if this variable stepping is not hourly, read only when time exceeds current time frame
        if (time > self%chunk_time_end) then
          self%chunk_time_start = self%chunk_time_end
          self%chunk_time_end = self%ds%times(self%ds%time_index(time))
          call self%ds%read_layered(self%var_id, self%cache(:, :, 1), time)
        end if
      end if
      exchange_var => self%cache(:, :, 1)  ! maybe not needed to re-associate every time
    end if
  end subroutine input_var2d_dp_update

  !> \brief Update a single 2D integer input variable.
  subroutine input_var2d_i4_update(self, exchange_var, time, chunking, end_time)
    class(input_var2d_i4), intent(inout), target :: self
    integer(i4), intent(inout), pointer :: exchange_var(:,:) !< exchange variable to update
    type(datetime), intent(in) :: time !< current model time
    integer(i4), intent(in) :: chunking !< chunking configuration (0 single read, -1 daily, -2 monthly, -3 yearly, >0 every n hours)
    type(datetime), intent(in) :: end_time !< end time of the simulation (for chunking limits)
    if (.not.self%provided) return
    if (self%check_couple_status()) return ! coupled variables are not updated here
    if (self%static) return ! static variables do not need to be updated
    ! check chunking
    if (chunking /= 1_i4) then
      ! update chunk
      if (time > self%chunk_time_end) then
        call update_time_frame(self%chunk_time_start, self%chunk_time_end, chunking, end_time)
        ! call message(" ... read new chunk: ", self%chunk_time_start%str(), " to ", self%chunk_time_end%str())
        nullify(exchange_var)
        if (allocated(self%cache)) deallocate(self%cache)
        call self%ds%read_chunk_layered(self%var_id, self%cache, self%chunk_time_start, self%chunk_time_end)
        self%offset = self%ds%time_index(self%chunk_time_start)
      end if
      exchange_var => self%cache(:, :, self%ds%time_index(time) - self%offset)
    else
      if (.not.allocated(self%cache)) allocate(self%cache(self%ds%grid%ncells, self%ds%nlayers, 1))
      if (self%stepping == 1_i4) then
        call self%ds%read_layered(self%var_id, self%cache(:, :, 1), time)
      else
        ! if this variable stepping is not hourly, read only when time exceeds current time frame
        if (time > self%chunk_time_end) then
          self%chunk_time_start = self%chunk_time_end
          self%chunk_time_end = self%ds%times(self%ds%time_index(time))
          call self%ds%read_layered(self%var_id, self%cache(:, :, 1), time)
        end if
      end if
      exchange_var => self%cache(:, :, 1)  ! maybe not needed to re-associate every time
    end if
  end subroutine input_var2d_i4_update

  !> \brief Read a single static double precision input variable and close the dataset.
  subroutine input_var_dp_read_static(self)
    class(input_var_dp), intent(inout) :: self
    if (.not.self%provided) return
    if (self%coupled) return
    if (.not.self%static) return
    if (.not.allocated(self%cache)) allocate(self%cache(self%ds%grid%ncells, 1))
    call self%ds%read(self%var_id, self%cache(:, 1))
    call self%ds%close()
  end subroutine input_var_dp_read_static

  !> \brief Read a single static integer input variable and close the dataset.
  subroutine input_var_i4_read_static(self)
    class(input_var_i4), intent(inout) :: self
    if (.not.self%provided) return
    if (self%coupled) return
    if (.not.self%static) return
    if (.not.allocated(self%cache)) allocate(self%cache(self%ds%grid%ncells, 1))
    call self%ds%read(self%var_id, self%cache(:, 1))
    call self%ds%close()
  end subroutine input_var_i4_read_static

  !> \brief Read a single static 2D double precision input variable and close the dataset.
  subroutine input_var2d_dp_read_static(self)
    class(input_var2d_dp), intent(inout) :: self
    if (.not.self%provided) return
    if (self%coupled) return
    if (.not.self%static) return
    if (.not.allocated(self%cache)) allocate(self%cache(self%ds%grid%ncells, self%ds%nlayers, 1))
    call self%ds%read_layered(self%var_id, self%cache(:, :, 1))
    call self%ds%close()
  end subroutine input_var2d_dp_read_static

  !> \brief Read a single static 2D integer input variable and close the dataset.
  subroutine input_var2d_i4_read_static(self)
    class(input_var2d_i4), intent(inout) :: self
    if (.not.self%provided) return
    if (self%coupled) return
    if (.not.self%static) return
    if (.not.allocated(self%cache)) allocate(self%cache(self%ds%grid%ncells, self%ds%nlayers, 1))
    call self%ds%read_layered(self%var_id, self%cache(:, :, 1))
    call self%ds%close()
  end subroutine input_var2d_i4_read_static

  !> \brief Reset the time frame for chunked reading.
  subroutine input_var_dp_reset_time(self, chunking, start_time)
    class(input_var_dp), intent(inout) :: self
    integer(i4), intent(in) :: chunking !< chunking mode
    type(datetime), intent(in) :: start_time !< start time of the simulation
    if (.not.self%provided) return
    if (self%static) return
    ! if read once and cache already allocated, do nothing
    if (.not.self%coupled .and. chunking == 0_i4 .and. allocated(self%cache)) return
    self%chunk_time_start = start_time
    self%chunk_time_end = start_time ! trigger read on first update or require coupler set
  end subroutine input_var_dp_reset_time

  !> \brief Reset the time frame for chunked reading.
  subroutine input_var_i4_reset_time(self, chunking, start_time)
    class(input_var_i4), intent(inout) :: self
    integer(i4), intent(in) :: chunking !< chunking mode
    type(datetime), intent(in) :: start_time !< start time of the simulation
    if (.not.self%provided) return
    if (self%static) return
    ! if read once and cache already allocated, do nothing
    if (.not.self%coupled .and. chunking == 0_i4 .and. allocated(self%cache)) return
    self%chunk_time_start = start_time
    self%chunk_time_end = start_time ! trigger read on first update or require coupler set
  end subroutine input_var_i4_reset_time

  !> \brief Reset the time frame for chunked reading.
  subroutine input_var2d_dp_reset_time(self, chunking, start_time)
    class(input_var2d_dp), intent(inout) :: self
    integer(i4), intent(in) :: chunking !< chunking mode
    type(datetime), intent(in) :: start_time !< start time of the simulation
    if (.not.self%provided) return
    if (self%static) return
    ! if read once and cache already allocated, do nothing
    if (.not.self%coupled .and. chunking == 0_i4 .and. allocated(self%cache)) return
    self%chunk_time_start = start_time
    self%chunk_time_end = start_time ! trigger read on first update or require coupler set
  end subroutine input_var2d_dp_reset_time

  !> \brief Reset the time frame for chunked reading.
  subroutine input_var2d_i4_reset_time(self, chunking, start_time)
    class(input_var2d_i4), intent(inout) :: self
    integer(i4), intent(in) :: chunking !< chunking mode
    type(datetime), intent(in) :: start_time !< start time of the simulation
    if (.not.self%provided) return
    if (self%static) return
    ! if read once and cache already allocated, do nothing
    if (.not.self%coupled .and. chunking == 0_i4 .and. allocated(self%cache)) return
    self%chunk_time_start = start_time
    self%chunk_time_end = start_time ! trigger read on first update or require coupler set
  end subroutine input_var2d_i4_reset_time

  !> \brief Initialize the input configuration.
  subroutine input_config_read(self, file)
    use nml_helper, only: NML_OK
    ! input/output variables
    class(input_config_t), target, intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelists
    character(1024) :: errmsg
    integer :: status
    status = self%input%from_file(file=file, errmsg=errmsg)
    if (status /= NML_OK) call error_message("Error reading input configuration from file: ", file, ", with error: ", trim(errmsg))
    ! TODO: read coupling configuration
  end subroutine input_config_read

  !> \brief Configure the Input container.
  !> \details Read configuration from file and initialize input variables.
  !! If file is not given, the configuration is assumed to be already set from a coupler,
  !! which is caught by the "is_configured" check.
  subroutine input_configure(self, file)
    class(input_t), target, intent(inout) :: self
    character(*), intent(in), optional :: file !< file containing the namelists
    integer :: status
    character(1024) :: errmsg
    integer(i4) :: id(1)
    call message(" ... configure input")
    if (present(file)) call self%config%read(file)
    if (.not.self%config%input%is_configured) call error_message("Input configuration not set.")
    status = self%config%input%is_valid(errmsg=errmsg)
    if (status /= NML_OK) call error_message("Input configuration not valid. Error: ", trim(errmsg))

    ! initialize general inputs settings
    id(1) = self%exchange%domain ! domain ID to read correct namelist entries
    self%chunking = self%config%input%chunking(id(1))
    self%time_stamp_location = self%config%input%time_stamp_location(id(1))

    ! morph mask (morph_mask_var by default "mask")
    status = self%config%input%is_set("morph_mask_path", idx=id, errmsg=errmsg)
    if (status == NML_OK) then
      call self%morph_mask%init( &
        path=self%config%input%morph_mask_path(id(1)), name=self%config%input%morph_mask_var(id(1)), static=.true.)
    end if

    ! DEM (dem_var by default "dem")
    status = self%config%input%is_set("dem_path", idx=id, errmsg=errmsg)
    if (status == NML_OK) then
      call self%dem%init( &
        path=self%config%input%dem_path(id(1)), name=self%config%input%dem_var(id(1)), static=.true.)
      self%exchange%dem%provided = .true. ! mark as provided in exchange
    end if

    ! slope (slope_var by default "slope")
    status = self%config%input%is_set("slope_path", idx=id, errmsg=errmsg)
    if (status == NML_OK) then
      call self%slope%init( &
        path=self%config%input%slope_path(id(1)), name=self%config%input%slope_var(id(1)), static=.true.)
      self%exchange%slope%provided = .true. ! mark as provided in exchange
    end if

    ! aspect (aspect_var by default "aspect")
    status = self%config%input%is_set("aspect_path", idx=id, errmsg=errmsg)
    if (status == NML_OK) then
      call self%aspect%init( &
        path=self%config%input%aspect_path(id(1)), name=self%config%input%aspect_var(id(1)), static=.true.)
      self%exchange%aspect%provided = .true. ! mark as provided in exchange
    end if

    ! flow direction (fdir_var by default "fdir")
    status = self%config%input%is_set("fdir_path", idx=id, errmsg=errmsg)
    if (status == NML_OK) then
      call self%fdir%init( &
        path=self%config%input%fdir_path(id(1)), name=self%config%input%fdir_var(id(1)), static=.true.)
      self%exchange%fdir%provided = .true. ! mark as provided in exchange
    end if

    ! flow accumulation (facc_var by default "facc")
    status = self%config%input%is_set("facc_path", idx=id, errmsg=errmsg)
    if (status == NML_OK) then
      call self%facc%init( &
        path=self%config%input%facc_path(id(1)), name=self%config%input%facc_var(id(1)), static=.true.)
      self%exchange%facc%provided = .true. ! mark as provided in exchange
    end if

    ! hydro mask (hydro_mask_var by default "mask")
    status = self%config%input%is_set("hydro_mask_path", idx=id, errmsg=errmsg)
    if (status == NML_OK) then
      call self%hydro_mask%init( &
        path=self%config%input%hydro_mask_path(id(1)), name=self%config%input%hydro_mask_var(id(1)), static=.true.)
    end if

    ! runoff (runoff_var by default "runoff")
    status = self%config%input%is_set("runoff_path", idx=id, errmsg=errmsg)
    if (status == NML_OK) then
      call self%runoff%init( &
        path=self%config%input%runoff_path(id(1)), name=self%config%input%runoff_var(id(1)))
      self%exchange%runoff_total%provided = .true. ! mark as provided in exchange
    end if
  end subroutine input_configure

  !> \brief Connect the Input container.
  !> \details Open datasets and initialize grids if needed.
  !! Principle: first come first serve for grid initialization.
  !! All static variables are read here and available afterwards for other containers.
  subroutine input_connect(self)
    class(input_t), target, intent(inout) :: self
    logical :: init_grid
    integer(i4) :: ts
    call message(" ... connecting input: ", self%exchange%time%str())
    ts = self%time_stamp_location

    ! morph mask
    if (self%morph_mask%coupled) then
      ! TODO: init grid from coupling namelist if needed
      call error_message("Input: morph mask is coupled... not yet implemented")
    else if (self%morph_mask%provided) then
      init_grid = need_grid(self%tgt_level0, self%exchange%level0) ! associate grid if not yet done
      call self%morph_mask%open_dataset(kind="i4", timestamp=ts, grid=self%exchange%level0, init_grid=init_grid)
      call self%morph_mask%ds%close() ! morph mask not read, since we only need the grid information
      call self%morph_mask%set_mask(self%exchange%level0%mask) ! only done for masks
    end if

    ! DEM
    if (self%dem%coupled) then
      ! TODO: init grid from coupling namelist if needed
      call error_message("Input: DEM is coupled... not yet implemented")
    else if (self%dem%provided) then
      init_grid = need_grid(self%tgt_level0, self%exchange%level0) ! associate grid if not yet done
      call self%dem%open_dataset(kind="dp", timestamp=ts, grid=self%exchange%level0, init_grid=init_grid)
      call self%dem%read_static()
      self%exchange%dem%data => self%dem%cache(:, 1) ! associate exchange variable to input cache
    end if

    ! slope
    if (self%slope%coupled) then
      ! TODO: init grid from coupling namelist if needed
      call error_message("Input: slope is coupled... not yet implemented")
    else if (self%slope%provided) then
      init_grid = need_grid(self%tgt_level0, self%exchange%level0) ! associate grid if not yet done
      call self%slope%open_dataset(kind="dp", timestamp=ts, grid=self%exchange%level0, init_grid=init_grid)
      call self%slope%read_static()
      self%exchange%slope%data => self%slope%cache(:, 1) ! associate exchange variable to input cache
    end if

    ! aspect
    if (self%aspect%coupled) then
      ! TODO: init grid from coupling namelist if needed
      call error_message("Input: aspect is coupled... not yet implemented")
    else if (self%aspect%provided) then
      init_grid = need_grid(self%tgt_level0, self%exchange%level0) ! associate grid if not yet done
      call self%aspect%open_dataset(kind="dp", timestamp=ts, grid=self%exchange%level0, init_grid=init_grid)
      call self%aspect%read_static()
      self%exchange%aspect%data => self%aspect%cache(:, 1) ! associate exchange variable to input cache
    end if

    ! flow direction
    if (self%fdir%coupled) then
      ! TODO: init grid from coupling namelist if needed
      call error_message("Input: flow direction is coupled... not yet implemented")
    else if (self%fdir%provided) then
      init_grid = need_grid(self%tgt_level0, self%exchange%level0) ! associate grid if not yet done
      call self%fdir%open_dataset(kind="i4", timestamp=ts, grid=self%exchange%level0, init_grid=init_grid)
      call self%fdir%read_static()
      self%exchange%fdir%data => self%fdir%cache(:, 1) ! associate exchange variable to input cache
    end if

    ! flow accumulation
    if (self%facc%coupled) then
      ! TODO: init grid from coupling namelist if needed
      call error_message("Input: flow accumulation is coupled... not yet implemented")
    else if (self%facc%provided) then
      init_grid = need_grid(self%tgt_level0, self%exchange%level0) ! associate grid if not yet done
      call self%facc%open_dataset(kind="i4", timestamp=ts, grid=self%exchange%level0, init_grid=init_grid)
      call self%facc%read_static()
      self%exchange%facc%data => self%facc%cache(:, 1) ! associate exchange variable to input cache
    end if

    ! hydro mask
    if (self%hydro_mask%coupled) then
        ! TODO: init grid from coupling namelist if needed
      call error_message("Input: hydro mask is coupled... not yet implemented")
    else if (self%hydro_mask%provided) then
      init_grid = need_grid(self%tgt_level1, self%exchange%level1) ! associate grid if not yet done
      call self%hydro_mask%open_dataset(kind="i4", timestamp=ts, grid=self%exchange%level1, init_grid=init_grid)
      call self%hydro_mask%ds%close() ! hydro mask not read, since we only need the grid information
      call self%hydro_mask%set_mask(self%exchange%level1%mask) ! only done for masks
    end if

    ! runoff
    if (self%runoff%coupled) then
        ! TODO: init grid from coupling namelist if needed
      call error_message("Input: runoff is coupled... not yet implemented")
    else if (self%runoff%provided) then
      init_grid = need_grid(self%tgt_level1, self%exchange%level1) ! associate grid if not yet done
      call self%runoff%open_dataset(kind="dp", timestamp=ts, grid=self%exchange%level1, init_grid=init_grid)
    end if
  end subroutine input_connect

  !> \brief Initialize the Input container for the model run.
  !> \details Prepare chunked reading if needed.
  subroutine input_initialize(self)
    class(input_t), target, intent(inout) :: self
    call message(" ... initialize input: ", self%exchange%time%str())
    ! TODO: warn if not required but provided
    self%runoff%provided = self%exchange%runoff_total%required ! only provide if required
    call self%runoff%reset_time(self%chunking, self%exchange%start_time)
  end subroutine input_initialize

  subroutine input_update(self)
    class(input_t), target, intent(inout) :: self
    call self%runoff%update(self%exchange%runoff_total%data, self%exchange%time, self%chunking, self%exchange%end_time)
  end subroutine input_update

  subroutine input_finalize(self)
    class(input_t), target, intent(inout) :: self
    call message(" ... finalize input")
    ! close datasets
    if (self%runoff%provided) call self%runoff%ds%close()
  end subroutine input_finalize

end module mo_input_container
