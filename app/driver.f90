!> \brief mHM next gen driver.
program driver
  use mo_cli, only: cli_parser
  use mo_os, only: path_abspath, path_join, check_path_isdir
  use mo_message, only: message
  use mo_mhm_cli, only: set_verbosity_level
  use mo_string_utils, only: n2s => num2str
  ! exchange
  use mo_domain, only: domains, selected_domains, domain_t
  use mo_kind, only: i4
  use mo_main_config, only: parameter_config_t, process_config_t, main_config_t, parameters_t
  use mo_exchange_type, only: time_config_t
  ! containers
  use mo_input_container, only: input_config_t
  use mo_meteo_container, only: meteo_config_t
  use mo_mpr_container, only: mpr_config_t
  use mo_mhm_container, only: mhm_config_t
  use mo_mrm_container, only: mrm_config_t

  integer(i4) :: n_domains, i, id
  character(len=:), allocatable :: cwd
  type(domain_t), pointer :: domain

  ! global configs
  type(main_config_t) :: main_cfg
  type(parameter_config_t) :: parameter_cfg
  type(process_config_t) :: process_cfg
  type(parameters_t) :: parameters
  type(time_config_t) :: time_cfg
  ! container configs
  type(input_config_t) :: input_cfg
  type(meteo_config_t) :: meteo_cfg
  type(mpr_config_t) :: mpr_cfg
  type(mhm_config_t) :: mhm_cfg
  type(mrm_config_t) :: mrm_cfg

  ! command line interface parser
  type(cli_parser) :: parser

  parser = cli_parser( &
    description="The mesoscale hydrological model - mHM v6", &
    add_logger_options=.true., &
    add_version_option=.true., &
    version="6.0")

  call parser%add_option( &
    name="cwd", &
    blank=.true., &
    help="The desired working directory (optional).")

  call parser%add_option( &
    name="nml", &
    s_name="n", &
    has_value=.true., &
    value_name="path", &
    default="mhm.nml", &
    help="The mHM configuration namelist.")

  call parser%add_option( &
    name="parameter", &
    s_name="p", &
    has_value=.true., &
    value_name="path", &
    default="mhm_parameter.nml", &
    help="The mHM parameter namelist.")

  call parser%add_option( &
    name="mhm_output", &
    s_name="o", &
    has_value=.true., &
    value_name="path", &
    default="mhm_outputs.nml", &
    help="The mHM output namelist.")

  call parser%add_option( &
    name="mrm_output", &
    s_name="r", &
    has_value=.true., &
    value_name="path", &
    default="mrm_outputs.nml", &
    help="The mRM output namelist.")

  ! parse given command line arguments
  cwd = "."
  call parser%parse()
  call set_verbosity_level(3_i4 - parser%option_read_count("quiet"))
  if (parser%option_was_read("cwd")) cwd = parser%option_value("cwd")
  cwd = path_abspath(cwd)
  call check_path_isdir(cwd, raise=.true.)

  call message("READ MAIN CONFIG")

  ! global configs
  call main_cfg%read(path_join(cwd, parser%option_value("nml")))
  call time_cfg%read(path_join(cwd, parser%option_value("nml")))
  call process_cfg%read(path_join(cwd, parser%option_value("nml")))
  call parameter_cfg%read(path_join(cwd, parser%option_value("parameter")))
  ! container configs
  call input_cfg%read(path_join(cwd, parser%option_value("nml")))
  call meteo_cfg%read(path_join(cwd, parser%option_value("nml")))
  call mpr_cfg%read(path_join(cwd, parser%option_value("nml")))
  call mhm_cfg%read(path_join(cwd, parser%option_value("nml")), path_join(cwd, parser%option_value("mhm_output")))
  call mrm_cfg%read(path_join(cwd, parser%option_value("nml")), path_join(cwd, parser%option_value("mrm_output")))

  ! create parameters first
  call message("")
  call message("CREATE PARAMETERS")
  call parameters%configure(parameter_cfg, process_cfg)

  ! determine number of domains
  n_domains = main_cfg%mainconfig%nDomains
  allocate(selected_domains(n_domains))

  call message("")
  call message("CREATE domains: ", trim(n2s(n_domains)))
  do i = 1_i4, n_domains
    selected_domains(i) = domains%add_domain() ! returns domain id
  end do
  print*, " ... selected_domains", selected_domains

  ! read configs
  do i = 1_i4, size(selected_domains)
    id = selected_domains(i)
    call message("")
    call message("CONFIGURE domain: ", trim(adjustl(n2s(id))))
    ! get domain
    call domains%get_domain(id, domain)
    call domain%configure(parameters, time_cfg, id, cwd, input_cfg, meteo_cfg, mpr_cfg, mhm_cfg, mrm_cfg)
  end do

  ! simple run
  do i = 1_i4, size(selected_domains)
    call message("")
    id = selected_domains(i)
    call domains%get_domain(id, domain)
    call message("PREPARE domain: ", trim(adjustl(n2s(id))))
    call domain%initialize()
    call message("RUN TIME LOOP domain: ", trim(adjustl(n2s(id))))
    do while(domain%exchange%time < domain%exchange%end_time)
      call domain%update()
    end do
    call message("FINALIZE domain: ", trim(adjustl(n2s(id))))
    call domain%finalize()
  end do

end program driver
