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
  use mo_kind, only: i4, dp
  use mo_nml, only: open_nml, position_nml ! , close_nml
  use mo_namelists, only: open_new_nml, close_nml
  use mo_exchange_type, only: exchange_t
  use mo_message, only: message, error_message
  !> \class   mrm_config_t
  !> \brief   Configuration for a single mRM process container.
  type, public :: mrm_config_t
    logical :: active = .false. !< flag to activate the mRM process container

    ! main config
    integer(i4)      :: chunk            ! reading chunk size: off (default - 0), monthly (1), yearly (2), once (not sure what this should be)
    integer(i4)      :: out_frequency    ! default="daily", help="Output frequency: hourly, daily (default), monthly, yearly, once")
    integer(i4)      :: level11          ! has_value=.true.,  help="Routing grid resolution. By default: Resolution of runoff.")
    character(1024) :: start_time       ! "t", has_value=.true.,  help="Simulation start time (YYYY-MM-DD[Thh:mm]). By default: Start of runoff.")
    character(1024) :: end_time         ! "e", has_value=.true.,  help="Simulation end time (YYYY-MM-DD[Thh:mm]). By default: End of runoff.")
    integer(i4)      :: omp_min          !`"m", has_value=.true.,  help="Minimum river level size to route in parallel with OpenMP. By default: threads * 8")
    integer(i4)      :: parallel_rout    ! "p", has_value=.false., help="Level order for parallel routing starting from river root.")
    ! directories and files
    character(1024) :: scc               ! scc gauge locations file. Either CSV with station per line or NetCDF. By default not used.
    character(1024) :: fdir
    character(1024) :: slope
    character(1024) :: runoff
    character(1024) :: out_file
    character(1024) :: node_out_file
    ! parameters
    real(dp) :: gamma
    real(dp) :: const_celerity

  contains
    procedure :: read => mrm_config_read
  end type mrm_config_t

  !> \class   mrm_t
  !> \brief   Class for a single mRM process container.
  type, public :: mrm_t
    type(mrm_config_t) :: config !< configuration of the mRM process container
    type(exchange_t), pointer :: exchange => null() !< exchange container of the domain
  contains
    procedure :: configure => mrm_configure
    procedure :: connect => mrm_connect
    procedure :: initialize => mrm_initialize
    procedure :: update => mrm_update
  end type mrm_t

contains

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
    integer(i4)      :: chunk            ! reading chunk size: off (default - 0), monthly (1), yearly (2), once (not sure what this should be)
    integer(i4)      :: out_frequency    ! default="daily", help="Output frequency: hourly, daily (default), monthly, yearly, once")
    integer(i4)      :: level11          ! has_value=.true.,  help="Routing grid resolution. By default: Resolution of runoff.")
    character(1024) :: start_time       ! "t", has_value=.true.,  help="Simulation start time (YYYY-MM-DD[Thh:mm]). By default: Start of runoff.")
    character(1024) :: end_time         ! "e", has_value=.true.,  help="Simulation end time (YYYY-MM-DD[Thh:mm]). By default: End of runoff.")
    integer(i4)      :: omp_min          !`"m", has_value=.true.,  help="Minimum river level size to route in parallel with OpenMP. By default: threads * 8")
    integer(i4)      :: parallel_rout    ! "p", has_value=.false., help="Level order for parallel routing starting from river root.")
    ! directories and files
    character(1024) :: scc               ! scc gauge locations file. Either CSV with station per line or NetCDF. By default not used.
    character(1024) :: fdir
    character(1024) :: slope
    character(1024) :: runoff
    character(1024) :: out_file
    character(1024) :: node_out_file
    ! parameters
    real(dp) :: gamma
    real(dp) :: const_celerity

    namelist /mrm_main/ chunk, out_fequency, level11, start_time, end_time, omp_min, parallel_rout
    namelist /mrm_dirs/ scc, fdir, slope, runoff, out_file, node_out_file
    namelist /mrm_params/ gamma, const_celerity

    call message(" ... read config mrm: ", file, ", ", output_file)
    self%active = .true.

    ! read main config
    print *, '***CAUTION: nml files hard-coded in mo_mrm_container'
    call open_new_nml('mhm.nml', unit)
    call position_nml('mrm_main', unit)
    read(unit, nml=mrm_main)
    self%chunk         = chunk         ! reading chunk size: off (default - 0), monthly (1), yearly (2), once (not sure what this should be)
    self%out_frequency = out_frequency ! default="daily", help="Output frequency: hourly, daily (default), monthly, yearly, once")
    self%level11       = level11       ! has_value=.true.,  help="Routing grid resolution. By default: Resolution of runoff.")
    self%start_time    = start_time    ! "t", has_value=.true.,  help="Simulation start time (YYYY-MM-DD[Thh:mm]). By default: Start of runoff.")
    self%end_time      = end_time      ! "e", has_value=.true.,  help="Simulation end time (YYYY-MM-DD[Thh:mm]). By default: End of runoff.")
    self%omp_min       = omp_min       !`"m", has_value=.true.,  help="Minimum river level size to route in parallel with OpenMP. By default: threads * 8")
    self%parallel_rout = parallel_rout ! "p", has_value=.false., help="Level order for parallel routing starting from river root.")

    ! read directories
    call position_nml('mrm_dirs', unit)
    read(unit, nml=mrm_dirs)
    call close_nml(unit)
    self%scc = scc
    self%fdir = fdir
    self%slope = slope
    self%runoff = runoff 
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
    class(mrm_t), intent(inout) :: self
    call message(" ... connecting mrm: ", self%exchange%time%str())

    ! read values
    ! <- runoff file if required
    ! <- fdir file
    ! <- slope file
    
    ! generate river
  ! call message("create river network:", n2s(level0%ncells))
  ! call river%from_fdir(fdir, level0)

  ! call message("calculate facc on level0")
  ! call river%calc_order()
  ! call river%calc_facc()

  ! call message("upscale river")
  ! if (cli%option_was_read("level11")) then
  !   tmp = cli%option_value("level11")
  !   read(tmp,*) resolution ! convert string to number
  ! else
  !   resolution = input%grid%cellsize
  ! end if

  ! scc = cli%option_was_read("scc")
  ! if (scc) then
  !  (scc_latlon)  ! if not isd, it is not present as optional argument
  !   call message(" ... read scc gauges file: ", cli%option_value("scc"))
  !   call read_scc_gauges(cli%option_value("scc"), scc_gauges, scc_latlon)
  !   print*, scc_latlon
  !   print*, scc_gauges
  ! end if
    ! populate exchange type

  end subroutine mrm_connect

  ! set initial values like timestep 0
  subroutine mrm_initialize(self)
    class(mrm_t), intent(inout) :: self
    call message(" ... initialize mrm: ", self%exchange%time%str())
  end subroutine mrm_initialize

  ! perform routing within time loop
  subroutine mrm_update(self)
    class(mrm_t), intent(inout) :: self
    call message(" ... updating mrm: ", self%exchange%time%str())
  end subroutine mrm_update

end module mo_mrm_container
