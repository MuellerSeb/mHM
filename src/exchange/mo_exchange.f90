!> \dir exchange
!> \brief \copybrief f_exchange
!> \details \copydetails f_exchange

!> \defgroup   f_exchange exchange - Fortran modules
!> \brief      Modules to deal with data exchange between mHM components.
!> \details    This module provides different types to enable the data exchange between components.

!> \file    mo_exchange.f90
!> \brief   \copybrief mo_exchange
!> \details \copydetails mo_exchange

!> \brief   Module to provide the exchange type.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Mar 2025
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_exchange
module mo_exchange
  use mo_common_types, only: grid_t => grid
  use mo_datetime, only: datetime
  use mo_kind, only: dp, i4
  use mo_message, only: error_message
  use mo_string_utils, only: n2s=>num2str
  implicit none
  private
  integer(i4), public, parameter :: dtype_dp = 0_i4 !< double precision data type.
  integer(i4), public, parameter :: dtype_i4 = 1_i4 !< 4 byte integer data type.
  integer(i4), public, parameter :: dtype_lg = 2_i4 !< logical data type.

  type, public :: variable_t
    character(:), allocatable :: name !< variable name
    character(:), allocatable :: unit !< variable name
    character(:), allocatable :: long_name !< long name of the variable
    character(:), allocatable :: standard_name !< standard name of the variable
    integer(i4) :: grid !< ID of the grid (in exchange container) the data is defined on
    type(datetime) :: timestamp !< time-stamp for the currently provided data
    logical :: static = .false. !< flag to indicated static data (.false. by default)
    logical :: provided = .false. !< flag to indicate that data is provided by a component (.false. by default)
    logical :: required = .false. !< flag to indicate that data is required by a component (.false. by default)
    integer(i4) :: dtype !< data type (either real (dtype_dp), integer (dtype_i4) or logical (dtype_lg))
    integer(i4) :: ndim  !< number of dimensions (1 or 2)
    real(dp), dimension(:), pointer :: ptr_1d_dp => null() !< 1D real pointer (n-cells)
    real(dp), dimension(:,:), pointer :: ptr_2d_dp => null() !< 2D real pointer (n-cells, horizons)
    integer(i4), dimension(:), pointer :: ptr_1d_i4 => null() !< 1D integer pointer (n-cells)
    integer(i4), dimension(:,:), pointer :: ptr_2d_i4 => null() !< 2D integer pointer (n-cells, horizons)
    logical, dimension(:), pointer :: ptr_1d_lg => null() !< 1D logical pointer (n-cells)
    logical, dimension(:,:), pointer :: ptr_2d_lg => null() !< 2D logical pointer (n-cells, horizons)
  end type variable_t

  type, public :: exchange_t
    type(grid_t), dimension(:), allocatable :: grids !< list of all grids used in the run
    type(variable_t), dimension(:), allocatable :: variables !< list of all variables exchanged in the run
    integer(i4) :: time_step = 0_i4 !< current time step
    type(datetime) :: current_time !< time-stamp for the current time step
  contains
    procedure, public  :: add_variable => exchange_add_variable
    procedure, public  :: add_grid => exchange_add_grid
    procedure, public  :: has_variable => exchange_has_variable
    procedure, public  :: get_variable_id => exchange_get_variable_id
    procedure, private :: check_timestamp => exchange_check_timestamp
    procedure, private :: exchange_get_data_1d_dp
    procedure, private :: exchange_get_data_2d_dp
    procedure, private :: exchange_get_data_1d_i4
    procedure, private :: exchange_get_data_2d_i4
    procedure, private :: exchange_get_data_1d_lg
    procedure, private :: exchange_get_data_2d_lg
    generic, public    :: get_data => exchange_get_data_1d_dp, exchange_get_data_2d_dp
    generic, public    :: get_data => exchange_get_data_1d_i4, exchange_get_data_2d_i4
    generic, public    :: get_data => exchange_get_data_1d_lg, exchange_get_data_2d_lg
    procedure, private :: exchange_set_data_1d_dp
    procedure, private :: exchange_set_data_2d_dp
    procedure, private :: exchange_set_data_1d_i4
    procedure, private :: exchange_set_data_2d_i4
    procedure, private :: exchange_set_data_1d_lg
    procedure, private :: exchange_set_data_2d_lg
    generic, public    :: set_data => exchange_set_data_1d_dp, exchange_set_data_2d_dp
    generic, public    :: set_data => exchange_set_data_1d_i4, exchange_set_data_2d_i4
    generic, public    :: set_data => exchange_set_data_1d_lg, exchange_set_data_2d_lg
  end type exchange_t

  contains

  !> \brief Add a new variable to the exchange container.
  !> \return ID of added variable as integer
  integer(i4) function exchange_add_variable(this, name, unit, dtype, grid, ndim, long_name, standard_name, static)
    class(exchange_t), intent(inout)    :: this  !< exchange container
    character(*), intent(in)           :: name  !< variable name
    character(*), intent(in)           :: unit  !< variable name
    integer(i4), intent(in)            :: dtype !< data type (either real (dtype_dp), integer (dtype_i4) or logical (dtype_lg))
    integer(i4), intent(in)            :: grid  !< ID of the associated grid
    integer(i4), optional, intent(in)  :: ndim  !< number of dimensions (1 (default), or 2)
    character(*), optional, intent(in) :: long_name !< long name of the variable
    character(*), optional, intent(in) :: standard_name !< standard name of the variable
    logical, optional, intent(in)      :: static !< flag to indicated static data (.false. by default)

    type(variable_t) :: add_variable

    if (this%has_variable(name)) call error_message('exchange: variable name "', trim(name), '" already present.')

    add_variable%name = trim(name)
    add_variable%unit = trim(unit)

    if (.not.any(dtype == [dtype_dp, dtype_i4, dtype_lg])) &
      call error_message("exchange: unknown dtype '", n2s(dtype), "' for variable '", trim(name), "'.")
    add_variable%dtype = dtype

    if (present(ndim)) then
      if (.not.any(ndim == [1_i4, 2_i4])) &
        call error_message("exchange: unsupported number of dimensions '", n2s(ndim), "' for variable '", trim(name), "'.")
      add_variable%ndim = ndim
    end if

    ! their presence can be check by allocation status afterwards
    if (present(long_name)) add_variable%long_name = trim(long_name)
    if (present(standard_name)) add_variable%standard_name = trim(standard_name)

    if (present(static)) add_variable%static = static

    if (allocated(this%variables)) then
      this%variables = [this%variables, add_variable]
    else
      allocate(this%variables(1))
      this%variables(1)=add_variable
    end if

    ! new variable is at the end of the array
    exchange_add_variable = size(this%variables)

  end function exchange_add_variable

  !> \brief Add a new grid to the exchange container.
  !> \return ID of added grid as integer
  integer(i4) function exchange_add_grid(this, grid)
    class(exchange_t), intent(inout) :: this  !< exchange container
    type(grid_t), intent(in)        :: grid !< grid to add

    if (allocated(this%grids)) then
      this%grids = [this%grids, grid]
    else
      allocate(this%grids(1))
      this%grids(1)=grid
    end if

    ! new grid is at the end of the array
    exchange_add_grid = size(this%grids)

  end function exchange_add_grid

  !> \brief Check if variable is present in the exchange container.
  !> \return .True. or .False.
  logical function exchange_has_variable(this, name)
    class(exchange_t), intent(in)  :: this  !< exchange container
    character(*), intent(in)      :: name  !< variable name
    exchange_has_variable = this%get_variable_id(name) > 0
  end function exchange_has_variable

  !> \brief Get variable ID
  !> \return variable ID as integer
  integer(i4) function exchange_get_variable_id(this, name, id, raise)
    class(exchange_t), intent(in)      :: this  !< exchange container
    character(*), intent(in)          :: name   !< variable name
    integer(i4), intent(in), optional :: id     !< id for the variable (will be determined by name if not present)
    logical, intent(in), optional     :: raise  !< flag to raise error if variable not present (default: .true.)

    logical :: raise_
    integer(i4) :: i, max_i

    raise_ = .true.
    if (present(raise)) raise_ = raise

    max_i = 0
    if (allocated(this%variables)) max_i = size(this%variables)

    if (present(id)) then
      if (id > max_i .or. id < 1) then
        exchange_get_variable_id = -1
      else
        if (trim(this%variables(id)%name) /= trim(name)) &
          call error_message('exchange: The variable name "', trim(name), '" has other ID than provided.')
        exchange_get_variable_id = id
      end if
    else
      exchange_get_variable_id = -1
      do i = 1, max_i
        if (trim(this%variables(i)%name) == trim(name)) then
          exchange_get_variable_id = i
          exit
        end if
      end do
    end if

    if (exchange_get_variable_id == -1 .and. raise_) &
      call error_message('exchange: The variable name "', trim(name), '" does not exist.')

  end function exchange_get_variable_id

  !> \brief Check timestamp for given variable
  subroutine exchange_check_timestamp(this, name, timestamp, id, new)
    class(exchange_t), target, intent(in) :: this  !< exchange container
    character(*), intent(in) :: name !< name of the variable
    type(datetime), intent(in), optional :: timestamp !< time stamp for the request
    integer(i4), intent(in), optional :: id !< id for the variable (will be determined by name if not present)
    logical, intent(in), optional :: new !< flag to indicate a new (to be written) time-stamp
    integer(i4) :: id_
    logical :: new_
    id_ = this%get_variable_id(name, id)
    new_ = .false.
    if (present(new)) new_ = new
    if (.not.this%variables(id_)%static) then
      if (.not.present(timestamp)) &
        call error_message('exchange: The variable "', trim(name), '" is not static, but no timestamp was provided.')
      if (.not.new_.and.(this%variables(id_)%timestamp /= timestamp)) &
        call error_message('exchange: The variable "', trim(name), '" has other timestamp than requested. ', &
        "Requested: ", timestamp%str() , " Provided: ", this%variables(id_)%timestamp%str())
    end if
    ! TODO: raise error if timestamp present but variable is static?
  end subroutine exchange_check_timestamp

  !> \brief Get data by setting pointer
  subroutine exchange_get_data_1d_dp(this, ptr, name, timestamp, id)
    class(exchange_t), target, intent(in) :: this  !< exchange container
    real(dp), dimension(:), pointer, intent(inout) :: ptr !< pointer to associate to the data
    character(*), intent(in) :: name !< name of the variable
    type(datetime), intent(in), optional :: timestamp !< time stamp for the request
    integer(i4), intent(in), optional :: id !< id for the variable (will be determined by name if not present)
    integer(i4) :: id_
    id_ = this%get_variable_id(name, id)

    if (this%variables(id_)%ndim /= 1_i4) &
      call error_message('exchange: The variable "', trim(name), '" is not 1D as requested.')
    if (this%variables(id_)%dtype /= dtype_dp) &
      call error_message('exchange: The variable "', trim(name), '" is not of real type as requested.')
    if (.not.associated(this%variables(id_)%ptr_1d_dp)) &
      call error_message('exchange: The variable "', trim(name), '" has no associated data.')
    call this%check_timestamp(name, timestamp, id_)
    ! set pointer
    ptr => this%variables(id_)%ptr_1d_dp
  end subroutine exchange_get_data_1d_dp

  !> \brief Get data by setting pointer
  subroutine exchange_get_data_2d_dp(this, ptr, name, timestamp, id)
    class(exchange_t), target, intent(in) :: this  !< exchange container
    real(dp), dimension(:,:), pointer, intent(inout) :: ptr !< pointer to associate to the data
    character(*), intent(in) :: name !< name of the variable
    type(datetime), intent(in), optional :: timestamp !< time stamp for the request
    integer(i4), intent(in), optional :: id !< id for the variable (will be determined by name if not present)
    integer(i4) :: id_
    id_ = this%get_variable_id(name, id)

    if (this%variables(id_)%ndim /= 2_i4) &
      call error_message('exchange: The variable "', trim(name), '" is not 2D as requested.')
    if (this%variables(id_)%dtype /= dtype_dp) &
      call error_message('exchange: The variable "', trim(name), '" is not of real type as requested.')
    if (.not.associated(this%variables(id_)%ptr_2d_dp)) &
      call error_message('exchange: The variable "', trim(name), '" has no associated data.')
    call this%check_timestamp(name, timestamp, id_)
    ! set pointer
    ptr => this%variables(id_)%ptr_2d_dp
  end subroutine exchange_get_data_2d_dp

  !> \brief Get data by setting pointer
  subroutine exchange_get_data_1d_i4(this, ptr, name, timestamp, id)
    class(exchange_t), target, intent(in) :: this  !< exchange container
    integer(i4), dimension(:), pointer, intent(inout) :: ptr !< pointer to associate to the data
    character(*), intent(in) :: name !< name of the variable
    type(datetime), intent(in), optional :: timestamp !< time stamp for the request
    integer(i4), intent(in), optional :: id !< id for the variable (will be determined by name if not present)
    integer(i4) :: id_
    id_ = this%get_variable_id(name, id)

    if (this%variables(id_)%ndim /= 1_i4) &
      call error_message('exchange: The variable "', trim(name), '" is not 1D as requested.')
    if (this%variables(id_)%dtype /= dtype_i4) &
      call error_message('exchange: The variable "', trim(name), '" is not of integer type as requested.')
    if (.not.associated(this%variables(id_)%ptr_1d_i4)) &
      call error_message('exchange: The variable "', trim(name), '" has no associated data.')
    call this%check_timestamp(name, timestamp, id_)
    ! set pointer
    ptr => this%variables(id_)%ptr_1d_i4
  end subroutine exchange_get_data_1d_i4

  !> \brief Get data by setting pointer
  subroutine exchange_get_data_2d_i4(this, ptr, name, timestamp, id)
    class(exchange_t), target, intent(in) :: this  !< exchange container
    integer(i4), dimension(:,:), pointer, intent(inout) :: ptr !< pointer to associate to the data
    character(*), intent(in) :: name !< name of the variable
    type(datetime), intent(in), optional :: timestamp !< time stamp for the request
    integer(i4), intent(in), optional :: id !< id for the variable (will be determined by name if not present)
    integer(i4) :: id_
    id_ = this%get_variable_id(name, id)

    if (this%variables(id_)%ndim /= 2_i4) &
      call error_message('exchange: The variable "', trim(name), '" is not 2D as requested.')
    if (this%variables(id_)%dtype /= dtype_i4) &
      call error_message('exchange: The variable "', trim(name), '" is not of integer type as requested.')
    if (.not.associated(this%variables(id_)%ptr_2d_i4)) &
      call error_message('exchange: The variable "', trim(name), '" has no associated data.')
    call this%check_timestamp(name, timestamp, id_)
    ! set pointer
    ptr => this%variables(id_)%ptr_2d_i4
  end subroutine exchange_get_data_2d_i4

  !> \brief Get data by setting pointer
  subroutine exchange_get_data_1d_lg(this, ptr, name, timestamp, id)
    class(exchange_t), target, intent(in) :: this  !< exchange container
    logical, dimension(:), pointer, intent(inout) :: ptr !< pointer to associate to the data
    character(*), intent(in) :: name !< name of the variable
    type(datetime), intent(in), optional :: timestamp !< time stamp for the request
    integer(i4), intent(in), optional :: id !< id for the variable (will be determined by name if not present)
    integer(i4) :: id_
    id_ = this%get_variable_id(name, id)

    if (this%variables(id_)%ndim /= 1_i4) &
      call error_message('exchange: The variable "', trim(name), '" is not 1D as requested.')
    if (this%variables(id_)%dtype /= dtype_lg) &
      call error_message('exchange: The variable "', trim(name), '" is not of logical type as requested.')
    if (.not.associated(this%variables(id_)%ptr_1d_lg)) &
      call error_message('exchange: The variable "', trim(name), '" has no associated data.')
    call this%check_timestamp(name, timestamp, id_)
    ! set pointer
    ptr => this%variables(id_)%ptr_1d_lg
  end subroutine exchange_get_data_1d_lg

  !> \brief Get data by setting pointer
  subroutine exchange_get_data_2d_lg(this, ptr, name, timestamp, id)
    class(exchange_t), target, intent(in) :: this  !< exchange container
    logical, dimension(:,:), pointer, intent(inout) :: ptr !< pointer to associate to the data
    character(*), intent(in) :: name !< name of the variable
    type(datetime), intent(in), optional :: timestamp !< time stamp for the request
    integer(i4), intent(in), optional :: id !< id for the variable (will be determined by name if not present)
    integer(i4) :: id_
    id_ = this%get_variable_id(name, id)

    if (this%variables(id_)%ndim /= 2_i4) &
      call error_message('exchange: The variable "', trim(name), '" is not 2D as requested.')
    if (this%variables(id_)%dtype /= dtype_lg) &
      call error_message('exchange: The variable "', trim(name), '" is not of logical type as requested.')
    if (.not.associated(this%variables(id_)%ptr_2d_lg)) &
      call error_message('exchange: The variable "', trim(name), '" has no associated data.')
    call this%check_timestamp(name, timestamp, id_)
    ! set pointer
    ptr => this%variables(id_)%ptr_2d_lg
  end subroutine exchange_get_data_2d_lg

  !> \brief Set data by giving a target
  subroutine exchange_set_data_1d_dp(this, tgt, name, timestamp, id)
    class(exchange_t), intent(inout) :: this  !< exchange container
    real(dp), dimension(:), target, intent(in) :: tgt !< target containing the data
    character(*), intent(in) :: name !< name of the variable
    type(datetime), intent(in), optional :: timestamp !< time stamp for the request
    integer(i4), intent(in), optional :: id !< id for the variable (will be determined by name if not present)
    integer(i4) :: id_
    id_ = this%get_variable_id(name, id)

    if (this%variables(id_)%ndim /= 1_i4) &
      call error_message('exchange: The variable "', trim(name), '" is not 1D as requested.')
    if (this%variables(id_)%dtype /= dtype_dp) &
      call error_message('exchange: The variable "', trim(name), '" is not of real type as requested.')
    call this%check_timestamp(name, timestamp, id_, new=.true.)
    if (present(timestamp)) this%variables(id_)%timestamp = timestamp
    this%variables(id_)%ptr_1d_dp => tgt
  end subroutine exchange_set_data_1d_dp

  !> \brief Set data by giving a target
  subroutine exchange_set_data_2d_dp(this, tgt, name, timestamp, id)
    class(exchange_t), intent(inout) :: this  !< exchange container
    real(dp), dimension(:,:), target, intent(in) :: tgt !< target containing the data
    character(*), intent(in) :: name !< name of the variable
    type(datetime), intent(in), optional :: timestamp !< time stamp for the request
    integer(i4), intent(in), optional :: id !< id for the variable (will be determined by name if not present)
    integer(i4) :: id_
    id_ = this%get_variable_id(name, id)

    if (this%variables(id_)%ndim /= 2_i4) &
      call error_message('exchange: The variable "', trim(name), '" is not 2D as requested.')
    if (this%variables(id_)%dtype /= dtype_dp) &
      call error_message('exchange: The variable "', trim(name), '" is not of real type as requested.')
    call this%check_timestamp(name, timestamp, id_, new=.true.)
    if (present(timestamp)) this%variables(id_)%timestamp = timestamp
    this%variables(id_)%ptr_2d_dp => tgt
  end subroutine exchange_set_data_2d_dp

  !> \brief Set data by giving a target
  subroutine exchange_set_data_1d_i4(this, tgt, name, timestamp, id)
    class(exchange_t), intent(inout) :: this  !< exchange container
    integer(i4), dimension(:), target, intent(in) :: tgt !< target containing the data
    character(*), intent(in) :: name !< name of the variable
    type(datetime), intent(in), optional :: timestamp !< time stamp for the request
    integer(i4), intent(in), optional :: id !< id for the variable (will be determined by name if not present)
    integer(i4) :: id_
    id_ = this%get_variable_id(name, id)

    if (this%variables(id_)%ndim /= 1_i4) &
      call error_message('exchange: The variable "', trim(name), '" is not 1D as requested.')
    if (this%variables(id_)%dtype /= dtype_i4) &
      call error_message('exchange: The variable "', trim(name), '" is not of integer type as requested.')
    call this%check_timestamp(name, timestamp, id_, new=.true.)
    if (present(timestamp)) this%variables(id_)%timestamp = timestamp
    this%variables(id_)%ptr_1d_i4 => tgt
  end subroutine exchange_set_data_1d_i4

  !> \brief Set data by giving a target
  subroutine exchange_set_data_2d_i4(this, tgt, name, timestamp, id)
    class(exchange_t), intent(inout) :: this  !< exchange container
    integer(i4), dimension(:,:), target, intent(in) :: tgt !< target containing the data
    character(*), intent(in) :: name !< name of the variable
    type(datetime), intent(in), optional :: timestamp !< time stamp for the request
    integer(i4), intent(in), optional :: id !< id for the variable (will be determined by name if not present)
    integer(i4) :: id_
    id_ = this%get_variable_id(name, id)

    if (this%variables(id_)%ndim /= 2_i4) &
      call error_message('exchange: The variable "', trim(name), '" is not 2D as requested.')
    if (this%variables(id_)%dtype /= dtype_i4) &
      call error_message('exchange: The variable "', trim(name), '" is not of integer type as requested.')
    call this%check_timestamp(name, timestamp, id_, new=.true.)
    if (present(timestamp)) this%variables(id_)%timestamp = timestamp
    this%variables(id_)%ptr_2d_i4 => tgt
  end subroutine exchange_set_data_2d_i4

  !> \brief Set data by giving a target
  subroutine exchange_set_data_1d_lg(this, tgt, name, timestamp, id)
    class(exchange_t), intent(inout) :: this  !< exchange container
    logical, dimension(:), target, intent(in) :: tgt !< target containing the data
    character(*), intent(in) :: name !< name of the variable
    type(datetime), intent(in), optional :: timestamp !< time stamp for the request
    integer(i4), intent(in), optional :: id !< id for the variable (will be determined by name if not present)
    integer(i4) :: id_
    id_ = this%get_variable_id(name, id)

    if (this%variables(id_)%ndim /= 1_i4) &
      call error_message('exchange: The variable "', trim(name), '" is not 1D as requested.')
    if (this%variables(id_)%dtype /= dtype_lg) &
      call error_message('exchange: The variable "', trim(name), '" is not of logical type as requested.')
    call this%check_timestamp(name, timestamp, id_, new=.true.)
    if (present(timestamp)) this%variables(id_)%timestamp = timestamp
    this%variables(id_)%ptr_1d_lg => tgt
  end subroutine exchange_set_data_1d_lg

  !> \brief Set data by giving a target
  subroutine exchange_set_data_2d_lg(this, tgt, name, timestamp, id)
    class(exchange_t), intent(inout) :: this  !< exchange container
    logical, dimension(:,:), target, intent(in) :: tgt !< target containing the data
    character(*), intent(in) :: name !< name of the variable
    type(datetime), intent(in), optional :: timestamp !< time stamp for the request
    integer(i4), intent(in), optional :: id !< id for the variable (will be determined by name if not present)
    integer(i4) :: id_
    id_ = this%get_variable_id(name, id)

    if (this%variables(id_)%ndim /= 2_i4) &
      call error_message('exchange: The variable "', trim(name), '" is not 2D as requested.')
    if (this%variables(id_)%dtype /= dtype_lg) &
      call error_message('exchange: The variable "', trim(name), '" is not of logical type as requested.')
    call this%check_timestamp(name, timestamp, id_, new=.true.)
    if (present(timestamp)) this%variables(id_)%timestamp = timestamp
    this%variables(id_)%ptr_2d_lg => tgt
  end subroutine exchange_set_data_2d_lg

end module mo_exchange
