!> \file    app/driver.F90
!> \copydoc driver

!> \brief mHM v6 driver.
!> \details This is the main driver program for mHM v6. It initializes the
!>          model, reads configurations, sets up domains, and runs the time loop.
!> \authors Sebastian Mueller
!> \date    Jan 2026
#include "logging.h"
program driver
  use mo_logging
  use mo_cli, only: cli_parser
  use mo_os, only: path_abspath, path_join, check_path_isdir
  use mo_message, only: message, error_message
  use mo_mhm_cli, only: set_verbosity_level
  use mo_string_utils, only: n2s => num2str
  use mo_domain, only: domains, selected_domains, domain_t
  use mo_kind, only: i4
  use nml_config_project, only: nml_config_project_t, NML_OK

  logical :: from_dirs
  integer(i4) :: n_domains, i, id
  character(len=:), allocatable :: cwd
  type(domain_t), pointer :: domain
  ! global configs
  type(nml_config_project_t) :: project
  ! command line interface parser
  type(cli_parser) :: parser

  character(:), allocatable :: meta_file, main_file, para_file, out_file
  character(1024) :: errmsg
  integer :: status

  parser = cli_parser(description="The mesoscale hydrological model - mHM v6", &
    add_logger_options=.true., add_version_option=.true., version="6.0")

  call parser%add_option(name="nml", s_name="n", has_value=.true., &
    value_name="path", default="mhm.nml", help="The mHM configuration namelist.")

  call parser%add_option(name="parameter", s_name="p", has_value=.true., &
    value_name="path", default="mhm_parameter.nml", help="The mHM parameter namelist.")

  call parser%add_option(name="output", s_name="o", has_value=.true., &
    value_name="path", default="outputs.nml", help="The mHM output namelist.")

  call parser%add_option(name="cwd", blank=.true., help="The desired working directory (optional).")

  ! parse given command line arguments
  call parser%parse()
  call set_verbosity_level(3_i4 - parser%option_read_count("quiet")) ! TODO: when switching to logging, not needed anymore

  ! get current working directory
  ! we don't change the process working directory, but use cwd for all relative paths
  ! we store the CWD in the exchange type later and use the "get_path" method to get paths
  cwd = "."
  if (parser%option_was_read("cwd")) cwd = parser%option_value("cwd")
  cwd = path_abspath(cwd)
  call check_path_isdir(cwd, raise=.true.)

  log_info(*) "READ MAIN CONFIG"

  ! global configs
  meta_file = path_join(cwd, parser%option_value("nml"))
  para_file = path_join(cwd, parser%option_value("parameter"))
  out_file  = path_join(cwd, parser%option_value("output"))
  status = project%from_file(file=meta_file, errmsg=errmsg)
  if (status /= NML_OK) call error_message("Error reading config_project from: ", meta_file, ", with error: ", trim(errmsg))
  status = project%is_valid(errmsg=errmsg)
  if (status /= NML_OK) call error_message("Project config not valid. Error: ", trim(errmsg))

  ! determine number of domains
  n_domains = project%n_domains
  from_dirs = project%read_domains_from_dirs
  main_file = meta_file
  if (from_dirs) main_file = "mhm.nml" ! default main file name in each domain directory
  allocate(selected_domains(n_domains))

  log_info(*) "CREATE domains: ", n_domains

  ! create domain-list
  ! we use a linked list to be able to dynamically add domains
  ! These domains are stored as allocated pointers, which have implicitly the "target" attribute
  ! so components can safely point to "exchange" of their domain
  do i = 1_i4, n_domains
    ! returns the list key (domain id) which can be used to get the domain later
    ! prepared for MPI runs with multiple domains
    selected_domains(i) = domains%add_domain() ! returns domain id
  end do

  log_debug(*) " ... selected_domains", selected_domains

  ! read configs
  do i = 1_i4, size(selected_domains)
    id = selected_domains(i)
    log_info(*) "CONFIGURE domain: ", id
    ! get domain
    call domains%get_domain(id, domain)
    ! id either from list or 1 if from dirs (always take domain 1 in each sub-dir)
    if (from_dirs) id = 1_i4
    ! create new domain and its exchange
    call domain%init(meta_file, main_file, para_file, id, cwd)
    ! configure domain components
    call domain%configure(main_file)
    ! check for connections and dependencies
    call domain%connect()
  end do

  ! simple run
  do i = 1_i4, size(selected_domains)
    id = selected_domains(i)
    call domains%get_domain(id, domain)
    log_info(*) "PREPARE domain: ", id
    call domain%initialize()
    log_info(*) "RUN TIME LOOP"
    do while(domain%exchange%time < domain%exchange%end_time)
      log_debug(*) " Time step: ", domain%exchange%time%str()
      call domain%update()
    end do
    log_info(*) "FINALIZE domain: ", id
    call domain%finalize()
  end do

end program driver
