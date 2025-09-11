!> \brief mHM next gen driver.
program driver
  use mo_cli, only: cli_parser
  use mo_os, only: change_dir
  use mo_message, only: message
  use mo_string_utils, only: n2s => num2str
  ! exchange
  use mo_domain, only: domains, selected_domains, domain_t, main_config_t
  use mo_kind, only: i4
  use mo_exchange_type, only: parameter_config_t, process_config_t, time_config_t
  ! containers
  use mo_input_container, only: input_config_t
  use mo_meteo_container, only: meteo_config_t
  use mo_mpr_container, only: mpr_config_t
  use mo_mhm_container, only: mhm_config_t
  use mo_mrm_container, only: mrm_config_t

  integer(i4) :: n_domains, i, id
  type(domain_t), pointer :: domain

  ! global configs
  type(main_config_t) :: main_cfg
  type(time_config_t), allocatable :: time_cfg(:)
  type(parameter_config_t), allocatable :: parameter_cfg(:)
  type(process_config_t), allocatable :: process_cfg(:)
  ! container configs
  type(input_config_t), allocatable :: input_cfg(:)
  type(meteo_config_t), allocatable :: meteo_cfg(:)
  type(mpr_config_t), allocatable :: mpr_cfg(:)
  type(mhm_config_t), allocatable :: mhm_cfg(:)
  type(mrm_config_t), allocatable :: mrm_cfg(:)

  ! command line interface parser
  type(cli_parser) :: parser

  parser = cli_parser( &
    description="The mesoscale hydrological model - mHM v6", &
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

  call parser%add_option( &
    name="quiet", &
    s_name="q", &
    repeated=.true., &
    help="Decrease verbosity level.")

  ! parse given command line arguments
  call parser%parse()
  if (parser%option_was_read("cwd")) call change_dir(parser%option_value("cwd"))

  call message("READ MAIN CONFIG")
  call main_cfg%read(parser%option_value("nml"))
  n_domains = main_cfg%mainconfig%nDomains
  allocate(selected_domains(n_domains))

  allocate(time_cfg(n_domains))
  allocate(parameter_cfg(n_domains))
  allocate(process_cfg(n_domains))
  ! container configs
  allocate(input_cfg(n_domains))
  allocate(meteo_cfg(n_domains))
  allocate(mpr_cfg(n_domains))
  allocate(mhm_cfg(n_domains))
  allocate(mrm_cfg(n_domains))

  call message("CREATE domains: ", trim(n2s(n_domains)))
  do i = 1_i4, n_domains
    selected_domains(i) = domains%add_domain() ! returns domain id
  end do
  print*, "selected_domains", selected_domains

  ! read configs
  do i = 1_i4, size(selected_domains)
    id = selected_domains(i)
    call message("READ CONFIGS domain: ", trim(adjustl(n2s(id))))
    ! global configs
    call time_cfg(id)%read(parser%option_value("nml"), id)
    call parameter_cfg(id)%read(parser%option_value("parameter"))
    call process_cfg(id)%read(parser%option_value("nml"))
    ! container configs
    call input_cfg(id)%read(parser%option_value("nml"), id)
    call meteo_cfg(id)%read(parser%option_value("nml"), id)
    call mpr_cfg(id)%read(parser%option_value("nml"), id)
    call mhm_cfg(id)%read(parser%option_value("nml"), parser%option_value("mhm_output"), id)
    call mrm_cfg(id)%read(parser%option_value("nml"), parser%option_value("mrm_output"), id)
    ! get domain
    call domains%get_domain(id, domain)
    call domain%init( &
      time_cfg(id), &
      parameter_cfg(id), &
      process_cfg(id), &
      input_cfg(id), &
      meteo_cfg(id), &
      mpr_cfg(id), &
      mhm_cfg(id), &
      mrm_cfg(id))
  end do

  ! simple run
  do i = 1_i4, size(selected_domains)
    id = selected_domains(i)
    call domains%get_domain(id, domain)
    call message("PREPARE domain: ", trim(adjustl(n2s(id))))
    call domain%prepare()
    call message("RUN TIME LOOP domain: ", trim(adjustl(n2s(id))))
    ! do while(domain%exchange%time < domain%exchange%end_time)
    call domain%update()
    ! end do
    call message("FINALIZE domain: ", trim(adjustl(n2s(id))))
    call domain%finalize()
  end do

end program driver
