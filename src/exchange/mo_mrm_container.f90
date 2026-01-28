!> \file    mo_mrm_container.f90
!> \brief   \copybrief mo_mrm_container
!> \details \copydetails mo_mrm_container

!> \brief   Module for a mHM process container.
!> \version 0.1
!> \authors Sebastian Mueller, Stephan Thober
!> \date    Aug 2025
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_exchange
!
! history
! Sep 2026 - Stephan Thober: initial version using river dag
module mo_mrm_container
  use mo_kind, only: i2, i4, i8, dp
  use mo_nml, only: position_nml ! , close_nml
  use mo_exchange_type, only: exchange_t
  use mo_message, only: message, error_message
  use mo_river, only: river_t
  use mo_river_router, only: river_router_t
  use mo_river_output, only: river_output_dataset
  use mo_grid, only: grid_t
  use mo_grid_io, only: output_dataset
  use mo_utils, only: is_close
  use mo_string_utils, only: n2s => num2str
  use mo_netcdf, only: NcDataset
  use nml_config_mrm, only: nml_config_mrm_t, NML_OK

  !> \class   mrm_t
  !> \brief   Class for a single mRM process container.
  type, public :: mrm_t
    type(nml_config_mrm_t)     :: config !< configuration of the mRM process container
    type(exchange_t), pointer  :: exchange => null() !< exchange container of the domain
    type(grid_t)               :: level11 !< mrm grid
    type(river_router_t)       :: router
    type(river_t)              :: river
    type(output_dataset)       :: ds_out
    type(river_output_dataset) :: dsr
    real(dp), allocatable      :: discharge(:)
    logical                    :: scc_active
    character(:), allocatable  :: output_path
  contains
    procedure :: configure => mrm_configure
    procedure :: connect => mrm_connect
    procedure :: initialize => mrm_initialize
    procedure :: update => mrm_update
    procedure :: finalize => mrm_finalize
    procedure :: read_restart => mrm_read_restart
    procedure :: write_restart => mrm_write_restart
  end type mrm_t

contains

  !> \brief Read restart file and populate mRM state.
  subroutine mrm_read_restart(self, file)
    use mo_string_utils, only: n2s => num2str
    use mo_datetime, only: one_hour
    class(mrm_t), target, intent(inout) :: self
    character(*), intent(in) :: file !< restart file to read

    type(NcDataset) :: restart_nc
    integer(i8), allocatable :: omp_min
    logical :: rout

    call error_message("mRM restart from file not yet implemented.")

    ! rout = self%config%parallel_rout
    ! if (self%config%omp_min .ge. 1_i4) then
    !   allocate(omp_min)
    !   omp_min = self%config%omp_min
    !   call message(" ... set minimum level size for openmp: ", n2s(omp_min))
    ! end if
    ! call message(" ... read mRM restart from file: ", file)
    ! restart_nc = NcDataset(self%exchange%get_path(file), "r")
    ! call self%level11%from_netcdf(restart_nc, "cell_area")
    ! self%river%grid => self%level11
    ! call self%river%init_from_restart(restart_nc=restart_nc)
    ! call self%router%init_from_restart( &
    !   restart_nc       = restart_nc, &
    !   river            = self%river, &
    !   input_grid       = self%exchange%level1, &
    !   input_step       = int(self%exchange%step/one_hour(), i4), &
    !   max_route_step   = 3600.0_dp, &
    !   root_levels      = rout, &
    !   omp_level_thresh = omp_min)
    ! call restart_nc%close()

  end subroutine mrm_read_restart

  subroutine mrm_write_restart(self, file)
    class(mrm_t), intent(inout) :: self
    character(*), intent(in) :: file !< restart file to write

    type(NcDataset) :: restart_nc

    restart_nc = NcDataset(self%exchange%get_path(file), "w")
    call message(" ... write mRM restart to file: ", file)
    call self%river%write_restart_to_dataset(restart_nc)
    call self%router%write_restart_to_dataset(restart_nc)
    call restart_nc%close()

  end subroutine mrm_write_restart

  !> \brief Configure the mRM process container.
  subroutine mrm_configure(self, file)
    class(mrm_t), intent(inout) :: self
    character(*), intent(in), optional :: file !< file containing the namelists
    character(1024) :: errmsg
    character(:), allocatable :: path
    integer :: status
    call message(" ... configure mrm")
    if (present(file)) then
      path = self%exchange%get_path(file) ! get absolute path relative to cwd
      call message(" ... read mRM config: ", path)
      status = self%config%from_file(file=path, errmsg=errmsg)
      if (status /= NML_OK) call error_message("Error reading mRM config from: ", path, ", with error: ", trim(errmsg))
    end if
    if (.not.self%config%is_configured) call error_message("mRM configuration not set.")
    status = self%config%is_valid(errmsg=errmsg)
    if (status /= NML_OK) call error_message("mRM config not valid. Error: ", trim(errmsg))
  end subroutine mrm_configure

  ! read initial values and populate exchange
  subroutine mrm_connect(self)
    use mo_datetime, only: datetime, timedelta, DAY_HOURS, one_hour, one_day
    use mo_grid, only: grid_t
    use mo_grid_io, only: var, input_dataset, output_dataset, center_timestamp, hourly, daily, monthly, yearly, time_units_delta
    use mo_river, only: river_t
    use mo_river_upscaler, only: river_upscaler_t
    use mo_river_tools, only: read_scc_gauges
    use mo_os, only: path_ext, path_isfile
    use mo_string_utils, only: n2s => num2str

    implicit none

    class(mrm_t), target, intent(inout) :: self
    type(river_t), target :: river_l0
    type(input_dataset) :: input, in_ds
    type(datetime) :: current_time, start_time_frame
    type(input_dataset) :: ds
    type(river_upscaler_t) :: upscaler
    logical                     :: rout
    logical, allocatable        :: scc_latlon
    character(:), allocatable   :: file, tmp, restart_in
    character(:), allocatable   :: delta
    integer(i4)                 :: write_step
    integer(i4)                 :: chunk_offset
    integer(i2), allocatable    :: fdir(:)
    integer(i4), allocatable    :: mfdir(:,:)
    integer(i8), allocatable    :: omp_min
    real(dp), allocatable       :: mdem(:,:), dem(:), mslope(:,:), slope(:), scc_gauges(:,:)
    real(dp), pointer           :: runoff(:) => null()

    integer(i4)                 :: id(1)
    logical                     :: const_celerity
    integer(i4)                 :: case
    real(dp), allocatable       :: gamma(:)

    integer :: status
    character(1024) :: errmsg

    call message(" ... connecting mrm: ", self%exchange%time%str())

    ! store domain id
    id(1) = self%exchange%domain

    case = self%exchange%parameters%config%processes%routing
    if (case == 1_i4) call error_message("mRM: process 'routing' set to 1, which is not implemented.")
    const_celerity = case == 2_i4

    self%exchange%runoff_total%required = .true.
    self%exchange%fdir%required = .true.
    self%exchange%slope%required = .not.const_celerity

    if (.not.self%exchange%runoff_total%provided) call error_message("mRM: runoff_total not provided.")
    if (.not.self%exchange%fdir%provided) call error_message("mRM: fdir not provided.")
    if (.not.const_celerity) then
      if (.not.self%exchange%slope%provided) call error_message("mRM: slope not provided, but required for variable celerity.")
    end if

    gamma = self%exchange%parameters%get_process(8_i4)  ! routing still process 8

    ! generate river
    if (self%config%read_restart(id(1))) then
      call error_message("mRM restart from file not yet implemented.")
      ! status = self%config%is_set("restart_input_path", idx=id, errmsg=errmsg)
      ! if (status /= NML_OK) call error_message("mRM restart input path not set for domain ", n2s(id(1)), ". Error: ", trim(errmsg))
      ! restart_in = self%exchange%get_path(self%config%restart_input_path(id(1)))
      ! call message("read river from restart file: ", restart_in)
      ! call self%read_restart(restart_in)
    else
      call message("create river network:", n2s(self%exchange%level0%ncells))
      ! TODO: make fdir i2
      call river_l0%from_fdir(int(self%exchange%fdir%data, i2), self%exchange%level0)

      call message("calculate facc on level0")
      call river_l0%calc_order()
      call river_l0%calc_facc()

      call message("upscale river")

      ! derive level11 grid
      status = self%config%is_set("resolution", idx=id, errmsg=errmsg)
      if (status /= NML_OK) call error_message("mRM resolution not set for domain ", n2s(id(1)), ". Error: ", trim(errmsg))
      self%level11 = self%exchange%level0%derive_grid(target_resolution=self%config%resolution(id(1)))
      self%exchange%level3 => self%level11  ! make level11 available as level3 in exchange container

      status = self%config%is_set("output_path", errmsg=errmsg)
      if (status /= NML_OK) call error_message("mRM output_path not set for domain ", n2s(id(1)), ". Error: ", trim(errmsg))

      self%output_path = self%exchange%get_path(self%config%output_path(id(1))) ! resolve relative path

      ! if (self%level11%has_aux_coords()) call self%level11%estimate_aux_vertices()
      call message(" ... level0 ncells", n2s(self%exchange%level0%ncells))
      call message(" ... level0 cellsize", n2s(self%exchange%level0%cellsize))
      call message(" ... level1 ncells", n2s(self%exchange%level1%ncells))
      call message(" ... level1 cellsize", n2s(self%exchange%level1%cellsize))
      call message(" ... level11 ncells", n2s(self%level11%ncells))
      call message(" ... level11 cellsize", n2s(self%level11%cellsize))

      ! TODO: the upscaler should handle also the case of no upscaling (level0 == level11)
      if (is_close(self%level11%cellsize, self%exchange%level0%cellsize)) then
        call message(" ... use L0 river")
        call self%river%from_fdir(fdir, self%level11)
        if (const_celerity) then
          call self%river%calc_celerity(gamma=gamma(1), constant_celerity=const_celerity)
        else
          call self%river%calc_celerity(gamma=gamma(1), slope=self%exchange%slope%data, constant_celerity=const_celerity)
        end if
      else
        call upscaler%init(river_l0, self%river, self%level11, scc_gauges, scc_latlon) ! scc_gauges/scc_latlon not present if not allocated
        if (const_celerity) then
          call upscaler%calc_celerity(gamma=gamma(1), constant_celerity=const_celerity)
        else
          call upscaler%calc_celerity(gamma=gamma(1), slope=self%exchange%slope%data, constant_celerity=const_celerity)
        end if
        call upscaler%destroy()
      end if
      call river_l0%clean()
    end if

    self%scc_active = .false.
    ! if (path_isfile(self%config%scc_file)) then
    !   self%scc_active = .true.
    !   allocate(scc_latlon)  ! if not isd, it is not present as optional argument
    !   call message(" ... read scc gauges file: ", self%config%scc_file)
    !   call read_scc_gauges(self%config%scc_file, scc_gauges, scc_latlon)
    !   ! print*, scc_latlon
    !   ! print*, scc_gauges
    ! else
    !   self%scc_active = .false.
    ! end if

    call message("initialize router")
    ! rout = self%config%parallel_rout
    ! if (self%config%omp_min .ge. 1_i4) then
    !   allocate(omp_min)
    !   omp_min = self%config%omp_min
    !   call message(" ... set minimum level size for openmp: ", n2s(omp_min))
    ! end if
    ! if (.not. self%config%read_restart) then
    call self%router%init( &
      river            = self%river, &
      input_grid       = self%exchange%level1, &
      input_step       = int(self%exchange%step/one_hour(), i4), &
      max_route_step   = 3600.0_dp) ! truncate routing steps to 1 hour
      ! root_levels      = rout, &
      ! omp_level_thresh = omp_min) ! omp_min not present if not allocated
    ! end if
    call message(" ... router%step: ", n2s(self%router%step))
    call message(" ... last level in parallel: ", n2s(self%router%last_parallel_level), "/", n2s(self%router%river%order%n_levels))

    ! prepare run
    allocate(self%discharge(self%river%n_nodes), source=0.0_dp)

    ! populate exchange type
    self%exchange%q_mod%data => self%discharge

  end subroutine mrm_connect

  ! set initial values like timestep 0
  subroutine mrm_initialize(self)
    use mo_grid_io, only: var, center_timestamp, hourly, daily, monthly, yearly, time_units_delta
    class(mrm_t), target, intent(inout) :: self
    integer(i4)                         :: write_step
    character(:), allocatable           :: delta
    type(var), allocatable              :: vars(:)
    call message(" ... initialize mrm: ", self%exchange%time%str())

    ! should be moved here from connect call self%router%init()

    ! create output
    call message("create output file: ", self%output_path)
    write_step = daily
    ! select case(self%config%out_frequency)
    !   case("hourly")
    !     write_step = hourly
    !     call message(" ... hourly output")
    !   case("daily")
    !     write_step = daily
    !     call message(" ... daily output")
    !   case("monthly")
    !     write_step = monthly
    !     call message(" ... monthly output")
    !   case("yearly")
    !     write_step = yearly
    !     call message(" ... yearly output")
    !   case("once")
    !     write_step = 0_i4
    !     call message(" ... output once at end of run")
    !   case default
    !     call error_message("Unknown value for 'out_frequency': ", self%config%out_frequency)
    ! end select
    delta = time_units_delta(write_step, center_timestamp)
    vars = [var(name="discharge", units="m3 s-1", avg=.true.)]
    call self%ds_out%init(path=self%output_path, &
          grid=self%level11, &
          vars=vars, &
          start_time=self%exchange%start_time, &
          delta=delta, &
          timestamp=center_timestamp)

    ! if (self%scc_active) then
    !   call message(" ... create node based output file: ", self%config%node_out_file)
    !   call self%dsr%init(path=self%config%node_out_file, &
    !     river=self%river, &
    !     vars=vars, &
    !     start_time=self%exchange%start_time, &
    !     delta=delta, &
    !     timestamp=center_timestamp)
    ! end if

  end subroutine mrm_initialize

  ! perform routing within time loop
  subroutine mrm_update(self)
    use mo_grid_io, only: hourly, daily, monthly, yearly
    class(mrm_t), target, intent(inout) :: self
    logical :: write_stamp
    ! call message(" ... updating mRM: ", self%exchange%time%str())

    ! route runoff
    call self%router%update(self%exchange%runoff_total%data, self%discharge)

    ! update output
    call self%ds_out%update("discharge", self%discharge)
    ! if (self%scc_active) then
    !   call self%dsr%update("discharge", self%discharge)
    !   call self%ds_out%update("discharge", self%river%select_cell_values(self%discharge))
    ! else
    !   call self%ds_out%update("discharge", self%discharge)
    ! end if

    ! write time-stamp depending on config
    write_stamp = .false.
    if (self%exchange%time%is_new_day()) write_stamp = .true.
    ! select case(self%config%out_frequency)
    !   case("hourly")
    !     write_stamp = .true.
    !   case("daily")
    !     if (self%exchange%time%is_new_day()) write_stamp = .true.
    !   case("monthly")
    !     if (self%exchange%time%is_new_month()) write_stamp = .true.
    !   case("yearly")
    !     if (self%exchange%time%is_new_year()) write_stamp = .true.
    !   case("once")
    !     if (self%exchange%time == self%exchange%end_time) write_stamp = .true.
    ! end select
    if (write_stamp) call self%ds_out%write(self%exchange%time)
    ! if (write_stamp .and. self%scc_active) call self%dsr%write(self%exchange%time)

  end subroutine mrm_update

  subroutine mrm_finalize(self)
    class(mrm_t), intent(inout) :: self

    call message("finalize ... mRM")
    ! if (self%config%write_restart) then
    !   call self%write_restart(self%config%restart_file_out)
    ! end if
    call message("close ... mRM")
    call self%ds_out%close()
    ! if (self%scc_active) call self%dsr%close()

  end subroutine mrm_finalize

end module mo_mrm_container
