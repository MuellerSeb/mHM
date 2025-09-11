!> \file    mo_domain.f90
!> \brief   \copybrief mo_domain
!> \details \copydetails mo_domain

!> \brief   Module for a domain container.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Aug 2025
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_exchange
module mo_domain
  use mo_list, only: list
  use mo_kind, only: i4, dp
  use mo_message, only: message, error_message
  use mo_exchange_type, only: exchange_t, time_config_t, process_config_t, parameter_config_t
  use mo_string_utils, only: n2s=>num2str
  use mo_datetime, only: datetime, timedelta
  use mo_namelists, only: nml_mainconfig_t
  ! containers
  use mo_input_container, only: input_t, input_config_t
  use mo_meteo_container, only: meteo_t, meteo_config_t
  use mo_mpr_container, only: mpr_t, mpr_config_t
  use mo_mhm_container, only: mhm_t, mhm_config_t
  use mo_mrm_container, only: mrm_t, mrm_config_t

  !> \class   main_config_t
  !> \brief   Main configuration.
  type, public :: main_config_t
    character(:), allocatable :: file    !< mhm namelist file
    type(nml_mainconfig_t) :: mainconfig !< mainconfig configuration
  contains
    procedure :: read => main_config_read
  end type main_config_t

  !> \class   domain_t
  !> \brief   Class for a single mHM domain.
  type, public :: domain_t
    type(exchange_t) :: exchange !< the exchange container with all exchanged variables for this domain
    type(input_t) :: input !< the input container providing inputs from file or couplers
    type(meteo_t) :: meteo !< the meteorology container providing processed meteorological data
    type(mpr_t) :: mpr !< the MPR container providing parameter fields for the process containers
    type(mhm_t) :: mhm !< the mHM process container calculating vertical hydrological processes
    type(mrm_t) :: mrm !< the mRM process container for routing related processes
    logical :: is_finished = .false. !< whether the time-loop on this domain is finished
  contains
    procedure :: init => domain_init
    procedure :: prepare => domain_prepare
    procedure :: update => domain_update
    procedure :: finalize => domain_finalize
  end type domain_t

  !> \class   domain_list
  !> \brief   Class to hold a list of all domains as a linked list of pointers.
  type, extends(list) :: domain_list
    integer(i4) :: counter = 0_i4 !< internal counter for added domains
  contains
    procedure :: get_domain => domain_list_get
    procedure :: add_domain => domain_list_add
  end type

  type(domain_list), public :: domains
  integer(i4), allocatable, public :: selected_domains(:)

contains

  !> \brief Get pointer to desired domain from domain list.
  subroutine domain_list_get(self, key, domain)
    class(domain_list), intent(in) :: self
    integer(i4), intent(in) :: key !< domain key
    type(domain_t), pointer, intent(out) :: domain !< pointer to desired domain
    class(*), pointer :: p
    call self%get(key, p)
    if (associated(p)) then
      select type (p)
        type is (domain_t)
          domain => p
        class default
          call error_message("Domain '", n2s(key), "' not a domain type.")
      end select
    else
      call error_message("Domain '", n2s(key), "' not present.")
    end if
  end subroutine domain_list_get

  !> \brief Add a new domain to the domain list.
  function domain_list_add(self, key) result(new_key)
    class(domain_list), intent(inout) :: self
    integer(i4), optional, intent(in) :: key !< domain key
    integer(i4) :: new_key !< key of the added domain
    type(domain_t) :: new_domain
    self%counter = self%counter + 1_i4
    if (present(key)) then
      new_key = key
    else
      new_key = self%counter
    end if
    call self%add_clone(new_key, new_domain)
  end function domain_list_add

  !> \brief Initialize a new domain.
  subroutine domain_init(self, time_cfg, parameter_cfg, process_cfg, input_cfg, meteo_cfg, mpr_cfg, mhm_cfg, mrm_cfg)
    class(domain_t), intent(inout) :: self
    type(time_config_t), intent(in) :: time_cfg !< configuration of the timing
    type(parameter_config_t), intent(in) :: parameter_cfg !< configuration for the parameters
    type(process_config_t), intent(in) :: process_cfg !< configuration for the processes
    type(input_config_t), intent(in), optional :: input_cfg !< configuration for the input container
    type(meteo_config_t), intent(in), optional :: meteo_cfg !< configuration for the meteo container
    type(mpr_config_t), intent(in), optional :: mpr_cfg !< configuration for the mpr container
    type(mhm_config_t), intent(in), optional :: mhm_cfg !< configuration for the mhm container
    type(mrm_config_t), intent(in), optional :: mrm_cfg !< configuration for the mrm container

    call message(" ... init domain")
    call self%exchange%init(time_cfg, parameter_cfg, process_cfg)
    if (present(input_cfg)) call self%input%init(input_cfg)
    if (present(meteo_cfg)) call self%meteo%init(meteo_cfg)
    if (present(mpr_cfg)) call self%mpr%init(mpr_cfg)
    if (present(mhm_cfg)) call self%mhm%init(mhm_cfg)
    if (present(mrm_cfg)) call self%mrm%init(mrm_cfg)

    if (self%input%config%active) call self%input%connect(self%exchange)
    if (self%meteo%config%active) call self%meteo%connect(self%exchange)
    if (self%mpr%config%active) call self%mpr%connect(self%exchange)
    if (self%mhm%config%active) call self%mhm%connect(self%exchange)
    if (self%mrm%config%active) call self%mrm%connect(self%exchange)
  end subroutine domain_init

  subroutine domain_prepare(self, parameters)
    class(domain_t), intent(inout) :: self
    !> a set of global parameter (gamma) to run mHM, DIMENSION [no. of global_Parameters]
    real(dp), dimension(:), optional, intent(in) :: parameters
    call message(" ... prepare domain")
    if (present(parameters)) self%exchange%parameters = parameters
    if (self%input%config%active) call self%input%prepare(self%exchange)
    if (self%meteo%config%active) call self%meteo%prepare(self%exchange)
    if (self%mpr%config%active) call self%mpr%prepare(self%exchange)
    if (self%mhm%config%active) call self%mhm%prepare(self%exchange)
    if (self%mrm%config%active) call self%mrm%prepare(self%exchange)
  end subroutine domain_prepare

  subroutine domain_update(self)
    class(domain_t), intent(inout) :: self
    call self%exchange%time%add(self%exchange%step)
    self%exchange%step_count = self%exchange%step_count + 1_i4
    if (self%input%config%active) call self%input%update(self%exchange)
    if (self%meteo%config%active) call self%meteo%update(self%exchange)
    if (self%mpr%config%active) call self%mpr%update(self%exchange)
    if (self%mhm%config%active) call self%mhm%update(self%exchange)
    if (self%mrm%config%active) call self%mrm%update(self%exchange)
  end subroutine domain_update

  subroutine domain_finalize(self)
    class(domain_t), intent(inout) :: self
    call message(" ... finalizing domain")
  end subroutine domain_finalize

  !> \brief Read main configuration.
  subroutine main_config_read(self, file)
    class(main_config_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelists
    call message(" ... config main: ", file)
    self%file = file
    call self%mainconfig%read(file)
  end subroutine main_config_read

end module mo_domain
