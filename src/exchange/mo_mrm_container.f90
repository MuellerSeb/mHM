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
  use mo_kind, only: i4, i8, dp
  use mo_nml, only: position_nml ! , close_nml
  use mo_namelists, only: open_new_nml, close_nml
  use mo_exchange_type, only: exchange_t
  use mo_message, only: message, error_message
  use mo_river, only: river_t
  use mo_river_router, only: river_router_t
  use mo_river_output, only: river_output_dataset
  use mo_grid, only: grid_t
  use mo_grid_io, only: output_dataset
  use mo_utils, only: is_close
  use mo_netcdf, only: NcDataset
  !> \class   mrm_config_t
  !> \brief   Configuration for a single mRM process container.
  type, public :: mrm_config_t
    logical :: active = .false. !< flag to activate the mRM process container

    ! main config
    character(1024)  :: out_frequency    ! default="daily", help="Output frequency: hourly, daily (default), monthly, yearly, once")
    integer(i4)      :: level11          ! has_value=.true.,  help="Routing grid resolution. By default: Resolution of runoff.")
    integer(i4)      :: omp_min          !`"m", has_value=.true.,  help="Minimum river level size to route in parallel with OpenMP. By default: threads * 8")
    logical          :: parallel_rout    ! "p", has_value=.false., help="Level order for parallel routing starting from river root.")
    logical          :: read_restart     ! has_value=.true., help="Read restart file.")
    logical          :: write_restart    ! has_value=.true., help="Write restart file.")
    ! directories and files
    character(1024) :: scc_file               ! scc gauge locations file. Either CSV with station per line or NetCDF. By default not used.
    character(1024) :: fdir_file
    character(1024) :: dem_file
    character(1024) :: slope_file
    character(1024) :: out_file
    character(1024) :: node_out_file
    character(1024) :: restart_file_in = "mrm_restart_in.nc"
    character(1024) :: restart_file_out = "mrm_restart_out.nc"
    ! parameters
    real(dp) :: gamma
    logical  :: const_celerity

  contains
    procedure :: read => mrm_config_read
  end type mrm_config_t

  !> \class   mrm_t
  !> \brief   Class for a single mRM process container.
  type, public :: mrm_t
    type(mrm_config_t)        :: config !< configuration of the mRM process container
    type(exchange_t), pointer :: exchange => null() !< exchange container of the domain
    type(grid_t)              :: level0, level11
    type(river_router_t)      :: router
    type(river_t)             :: criver
    type(river_t)             :: river
    type(output_dataset)      :: ds_out
    type(river_output_dataset)      :: dsr
    real(dp), allocatable     :: discharge(:)
    logical                   :: scc_active
  contains
    procedure :: configure => mrm_configure
    procedure :: connect => mrm_connect
    procedure :: initialize => mrm_initialize
    procedure :: update => mrm_update
    procedure :: close ! don't we need a close
    procedure :: finalize
    procedure :: read_restart => mrm_read_restart
    procedure :: write_restart => mrm_write_restart
  end type mrm_t

contains

  !> \brief Read restart file and populate mRM state.
  subroutine mrm_read_restart(self, file)
    use mo_string_utils, only: n2s => num2str
    class(mrm_t), target, intent(inout) :: self
    character(*), intent(in) :: file !< restart file to read

    type(NcDataset) :: restart_nc
    integer(i8), allocatable :: omp_min
    logical :: rout

    rout = self%config%parallel_rout
    if (self%config%omp_min .ge. 1_i4) then
      allocate(omp_min)
      omp_min = self%config%omp_min
      call message(" ... set minimum level size for openmp: ", n2s(omp_min))
    end if
    call message(" ... read mRM restart from file: ", file)
    restart_nc = NcDataset(trim(file), "r")
    call self%level11%from_netcdf(restart_nc, "cell_area")
    call self%criver%init_from_restart(restart_nc, self%level11)
    call self%router%init_from_restart(restart_nc, self%criver, self%exchange%level1, self%exchange%runoff_total%stepping, max_route_step=3600.0_dp, root_levels=rout, omp_level_thresh=omp_min)
    call restart_nc%close()

  end subroutine mrm_read_restart

  subroutine mrm_write_restart(self, file)
    class(mrm_t), intent(inout) :: self
    character(*), intent(in) :: file !< restart file to write

    type(NcDataset) :: restart_nc

    restart_nc = NcDataset(trim(file), "w")
    call message(" ... write mRM restart to file: ", file)
    call self%criver%write_restart_to_dataset(restart_nc)
    call self%router%write_restart_to_dataset(restart_nc)
    call restart_nc%close()

  end subroutine mrm_write_restart

  !> \brief Configure the mRM process container.
  subroutine mrm_configure(self, config, exchange)
    class(mrm_t), intent(inout) :: self
    type(mrm_config_t), intent(in) :: config !< initialization config for mRM
    type(exchange_t), intent(in), pointer :: exchange !< exchange container of the domain
    call message(" ... configure mrm")
    self%config = config
    self%exchange => exchange
  end subroutine mrm_configure

  !> \brief Initialize the mrm configuration.
  subroutine mrm_config_read(self, file, output_file)
    use mo_file, only: file_namelist_mhm, file_namelist_mhm_param 

    class(mrm_config_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelists
    character(*), intent(in) :: output_file !< file containing the output namelist
    ! local variables
    integer(i4)      :: unit
    ! main config
    character(1024)  :: out_frequency    ! default="daily", help="Output frequency: hourly, daily (default), monthly, yearly, once")
    integer(i4)      :: level11          ! has_value=.true.,  help="Routing grid resolution. By default: Resolution of runoff.")
    integer(i4)      :: omp_min          !`"m", has_value=.true.,  help="Minimum river level size to route in parallel with OpenMP. By default: threads * 8")
    logical          :: parallel_rout    ! "p", has_value=.false., help="Level order for parallel routing starting from river root.")
    logical          :: read_restart     ! 
    logical          :: write_restart    !
    ! directories and files
    character(1024) :: scc_file               ! scc gauge locations file. Either CSV with station per line or NetCDF. By default not used.
    character(1024) :: fdir_file
    character(1024) :: dem_file
    character(1024) :: slope_file
    character(1024) :: out_file
    character(1024) :: node_out_file
    ! parameters
    real(dp) :: gamma
    logical  :: const_celerity

    namelist /mrm_main/ out_frequency, level11, omp_min, parallel_rout, read_restart, write_restart
    namelist /mrm_dirs/ scc_file, fdir_file, dem_file, slope_file, out_file, node_out_file
    namelist /mrm_params/ gamma, const_celerity

    call message(" ... read config mrm: ", file, ", ", output_file)
    self%active = .true.

    ! read main config
    print *, '***CAUTION: nml files hard-coded in mo_mrm_container'
    call open_new_nml('mhm.nml', unit)
    call position_nml('mrm_main', unit)
    read(unit, nml=mrm_main)
    self%out_frequency = out_frequency ! default="daily", help="Output frequency: hourly, daily (default), monthly, yearly, once")
    self%level11       = level11       ! has_value=.true.,  help="Routing grid resolution. By default: Resolution of runoff.")
    self%omp_min       = omp_min       !`"m", has_value=.true.,  help="Minimum river level size to route in parallel with OpenMP. By default: threads * 8")
    self%parallel_rout = parallel_rout ! "p", has_value=.false., help="Level order for parallel routing starting from river root.")
    self%read_restart  = read_restart  ! has_value=.true., help="Read restart file.")
    self%write_restart = write_restart ! has_value=.true., help="Write restart file.")

    ! read directories
    call position_nml('mrm_dirs', unit)
    read(unit, nml=mrm_dirs)
    call close_nml(unit)
    self%scc_file = scc_file
    self%fdir_file = fdir_file
    self%dem_file = dem_file
    self%slope_file = slope_file
    self%out_file = out_file
    self%node_out_file = node_out_file

    ! read parameters
    call open_new_nml('mhm_parameter.nml', unit)
    call position_nml('mrm_params', unit)
    read(unit, nml=mrm_params)
    call close_nml(unit)
    self%gamma = gamma
    self%const_celerity = const_celerity

  end subroutine mrm_config_read

  ! read initial values and populate exchange
  subroutine mrm_connect(self)
    use mo_datetime, only: datetime, timedelta, HOUR_SECONDS, DAY_HOURS, one_hour, one_day 
    use mo_grid, only: grid_t
    use mo_grid_io, only: var, input_dataset, output_dataset, center_timestamp, hourly, daily, monthly, yearly, time_units_delta
    use mo_river, only: river_t
    use mo_river_upscaler, only: river_upscaler_t
    use mo_river_tools, only: read_scc_gauges
    use mo_os, only: path_ext, path_isfile
    use mo_string_utils, only: n2s => num2str

    implicit none

    class(mrm_t), target, intent(inout) :: self
    type(input_dataset) :: input, in_ds
    type(datetime) :: current_time, start_time_frame 
    type(input_dataset) :: ds
    type(river_upscaler_t) :: upscaler
    logical                     :: rout
    logical, allocatable        :: scc_latlon
    character(:), allocatable   :: file, tmp
    character(:), allocatable   :: delta
    integer(i4)                 :: write_step
    integer(i4)                 :: chunk_offset
    integer(i4), allocatable    :: mfdir(:,:), fdir(:)
    integer(i8), allocatable    :: omp_min
    real(dp), allocatable       :: mdem(:,:), dem(:), mslope(:,:), slope(:), scc_gauges(:,:)
    real(dp), pointer           :: runoff(:) => null()

    call message(" ... connecting mrm: ", self%exchange%time%str())

    file = self%config%fdir_file
    call message("read data: ", trim(file))
    select case(path_ext(file))
      case(".nc")
        call ds%init(path=file, grid=self%level0, vars=[var(name="fdir", static=.true.)], grid_init_var="fdir")
        allocate(fdir(self%level0%ncells))
        call ds%read("fdir", fdir)
        call ds%close()
      case(".asc")
        call self%level0%from_ascii_file(file)
        call self%level0%read_data(file, mfdir)
        fdir = self%level0%pack(mfdir)
        deallocate(mfdir)
      case default
        call error_message("unknown file extension (i.e. not '.asc' or '.nc'): ", path_ext(file))
    end select
    
    if (.true.) then ! should only be read if celerity is not constant
      file = self%config%fdir_file
      call message("read slope: ", file)
      select case(path_ext(file))
        case(".nc")
          call in_ds%init(path=file, grid=self%level0, vars=[var(name="slope", static=.true.)])
          allocate(slope(self%level0%ncells))
          call in_ds%read("slope", slope)
          call in_ds%close()
        case(".asc")
          call self%level0%read_data(file, mslope)
          slope = self%level0%pack(mslope)
          deallocate(mslope)
        case default
          call error_message("unknown file extension (i.e. not '.asc' or '.nc'): ", path_ext(file))
      end select
  end if

  ! generate river
  if (self%config%read_restart) then
    if (.not. path_isfile(self%config%restart_file_in)) then
      call error_message("restart file not found: ", self%config%restart_file_in)
    end if
    call message("read river from restart file: ", self%config%restart_file_in)
    call self%read_restart(self%config%restart_file_in)
  else
    call message("create river network:", n2s(self%level0%ncells))
    call self%river%from_fdir(fdir, self%level0)

    call message("calculate facc on level0")
    call self%river%calc_order()
    call self%river%calc_facc()

    call message("upscale river")
  
    ! derive level11 grid
    self%level11 = self%level0%derive_grid(target_resolution=real(self%config%level11, dp))
    if (self%level11%has_aux_coords()) call self%level11%estimate_aux_vertices()
    call message(" ... level0 ncells", n2s(self%level0%ncells))
    call message(" ... level0 cellsize", n2s(self%level0%cellsize))
    call message(" ... level1 ncells", n2s(self%exchange%level1%ncells))
    call message(" ... level1 cellsize", n2s(self%exchange%level1%cellsize))
    call message(" ... level11 ncells", n2s(self%level11%ncells))
    call message(" ... level11 cellsize", n2s(self%level11%cellsize))
    if (is_close(self%level11%cellsize, self%level0%cellsize)) then
      call message(" ... use L0 river")
      call self%criver%from_fdir(fdir, self%level11)
      call self%criver%calc_celerity(gamma=self%config%gamma, slope=slope, constant_celerity=self%config%const_celerity)
    else
      call upscaler%init(self%river, self%criver, self%level11, scc_gauges, scc_latlon) ! scc_gauges/scc_latlon not present if not allocated
      call upscaler%calc_celerity(gamma=self%config%gamma, slope=slope, constant_celerity=self%config%const_celerity)
    end if
  end if
  
  if (path_isfile(self%config%scc_file)) then
    self%scc_active = .true.
    allocate(scc_latlon)  ! if not isd, it is not present as optional argument
    call message(" ... read scc gauges file: ", self%config%scc_file)
    call read_scc_gauges(self%config%scc_file, scc_gauges, scc_latlon)
    ! print*, scc_latlon
    ! print*, scc_gauges
  else
    self%scc_active = .false.
  end if

  ! TODO: destroy river and upscaler to save memory
  call message("initialize router")
  rout = self%config%parallel_rout
  if (self%config%omp_min .ge. 1_i4) then
    allocate(omp_min)
    omp_min = self%config%omp_min
    call message(" ... set minimum level size for openmp: ", n2s(omp_min))
  end if
  if (.not. self%config%read_restart) then
    call self%router%init(self%criver, self%exchange%level1, self%exchange%runoff_total%stepping, max_route_step=3600.0_dp, root_levels=rout, omp_level_thresh=omp_min) ! omp_min not present if not allocated
  end if
  call message(" ... router%step: ", n2s(self%router%step))
  call message(" ... last level in parallel: ", n2s(self%router%last_parallel_level), "/", n2s(self%router%river%order%n_levels))

  ! prepare run
  allocate(self%discharge(self%criver%n_nodes), source=0.0_dp)

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
    call message("create output file: ", self%config%out_file)
    select case(self%config%out_frequency)
      case("hourly")
        write_step = hourly
        call message(" ... hourly output")
      case("daily")
        write_step = daily
        call message(" ... daily output")
      case("monthly")
        write_step = monthly
        call message(" ... monthly output")
      case("yearly")
        write_step = yearly
        call message(" ... yearly output")
      case("once")
        write_step = 0_i4
        call message(" ... output once at end of run")
      case default
        call error_message("Unknown value for 'out_frequency': ", self%config%out_frequency)
    end select
    delta = time_units_delta(write_step, center_timestamp)
    vars = [var(name="discharge", units="m3 s-1", avg=.true.)]
    call self%ds_out%init(path=self%config%out_file, &
          grid=self%level11, &
          vars=vars, &
          start_time=self%exchange%start_time, &
          delta=delta, &
          timestamp=center_timestamp)

    if (self%scc_active) then
      call message(" ... create node based output file: ", self%config%node_out_file)
      call self%dsr%init(path=self%config%node_out_file, &
        river=self%criver, &
        vars=vars, &
        start_time=self%exchange%start_time, &
        delta=delta, &
        timestamp=center_timestamp)
  end if
  
  end subroutine mrm_initialize

  ! perform routing within time loop
  subroutine mrm_update(self)
    use mo_grid_io, only: hourly, daily, monthly, yearly
    class(mrm_t), target, intent(inout) :: self
    logical :: write_stamp
    call message(" ... updating mRM: ", self%exchange%time%str())
 
    ! route runoff
    ! print *, self%router%input_grid%ncells
    call self%router%update(self%exchange%runoff_total%data, self%discharge)
    
    ! update output
    if (self%scc_active) then
     call self%dsr%update("discharge", self%discharge)
      call self%ds_out%update("discharge", self%criver%select_cell_values(self%discharge))
    else
      call self%ds_out%update("discharge", self%discharge)
    end if

    ! write time-stamp depending on config
    write_stamp = .false.
    select case(self%config%out_frequency)
      case("hourly")
        write_stamp = .true.
      case("daily")
        if (self%exchange%time%is_new_day()) write_stamp = .true.
      case("monthly")
        if (self%exchange%time%is_new_month()) write_stamp = .true.
      case("yearly")
        if (self%exchange%time%is_new_year()) write_stamp = .true.
      case("once")
        if (self%exchange%time == self%exchange%end_time) write_stamp = .true.
    end select
    ! print *, self%exchange%time, self%config%out_frequency, write_stamp, self%exchange%start_time, self%exchange%end_time
    if (write_stamp) call self%ds_out%write(self%exchange%time)
    if (write_stamp .and. self%scc_active) call self%dsr%write(self%exchange%time)

  end subroutine mrm_update

  subroutine close(self)
    class(mrm_t), intent(inout) :: self

    call message("close ... mRM")
  ! if (write_step == 0_i4) then
  !   call ds%write(current_time)
  !   if (node_out) call dsr%write(current_time)
  ! end if
  ! call ds%close()
  ! if (node_out) call dsr%close()
  ! call input%close()

  ! ! destroy runoff pointer
  ! if (.not.chunking) deallocate(runoff)
  ! nullify(runoff)

  end subroutine close

  subroutine finalize(self)
    class(mrm_t), intent(inout) :: self

    call message("finalize ... mRM")
    if (self%config%write_restart) then
      call self%write_restart(self%config%restart_file_out)
    end if

  end subroutine finalize

end module mo_mrm_container
