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
  use mo_exchange_type, only: exchange_t, time_config_t
  use mo_string_utils, only: n2s=>num2str
  use mo_datetime, only: datetime, timedelta
  ! config
  use mo_main_config, only: parameters_t
  ! containers
  use mo_input_container, only: input_t, input_config_t
  use mo_meteo_container, only: meteo_t, meteo_config_t
  use mo_mpr_container, only: mpr_t, mpr_config_t
  use mo_mhm_container, only: mhm_t, mhm_config_t
  use mo_mrm_container, only: mrm_t, mrm_config_t

  !> \class   domain_t
  !> \brief   Class for a single mHM domain.
  !> \details Always add the "target" attribute to instances of domain, or use allocated pointers (have implicit target attribute).
  type, public :: domain_t
    type(exchange_t) :: exchange !< the exchange container with all exchanged variables for this domain
    type(input_t) :: input !< the input container providing inputs from file or couplers
    type(meteo_t) :: meteo !< the meteorology container providing processed meteorological data
    type(mpr_t) :: mpr !< the MPR container providing parameter fields for the process containers
    type(mhm_t) :: mhm !< the mHM process container calculating vertical hydrological processes
    type(mrm_t) :: mrm !< the mRM process container for routing related processes
    logical :: is_finished = .false. !< whether the time-loop on this domain is finished
  contains
    procedure :: configure => domain_configure
    procedure :: initialize => domain_initialize
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
    type(domain_t), save :: new_domain ! template for the new domain to be added (saved to avoid reallocation on each call)
    self%counter = self%counter + 1_i4
    if (present(key)) then
      new_key = key
    else
      new_key = self%counter
    end if
    call self%add_clone(new_key, new_domain)
  end function domain_list_add

  !> \brief Configure the domain.
  subroutine domain_configure(self, parameters, time_cfg, domain, cwd, input_cfg, meteo_cfg, mpr_cfg, mhm_cfg, mrm_cfg)
    ! domain is always an item of a domain_list, which stores "allocated pointers" and these implicitly have the "target" attribute
    class(domain_t), intent(inout), target :: self ! needs "target" so components can safely point to "exchange"
    type(parameters_t), intent(in) :: parameters !< configuration for the parameters
    type(time_config_t), intent(in) :: time_cfg !< configuration of the timing
    integer(i4), intent(in), optional :: domain !< domain ID of the current domain in the configuration arrays (1 by default)
    character(len=*), intent(in), optional :: cwd !< current working directory to set relative paths
    type(input_config_t), intent(in), optional :: input_cfg !< configuration for the input container
    type(meteo_config_t), intent(in), optional :: meteo_cfg !< configuration for the meteo container
    type(mpr_config_t), intent(in), optional :: mpr_cfg !< configuration for the mpr container
    type(mhm_config_t), intent(in), optional :: mhm_cfg !< configuration for the mhm container
    type(mrm_config_t), intent(in), optional :: mrm_cfg !< configuration for the mrm container

    call message(" ... configure domain")
    call self%exchange%configure(time_cfg, parameters, domain, cwd)
    ! configure
    if (present(input_cfg)) call self%input%configure(input_cfg, self%exchange)
    if (present(meteo_cfg)) call self%meteo%configure(meteo_cfg, self%exchange)
    if (present(mpr_cfg)) call self%mpr%configure(mpr_cfg, self%exchange)
    if (present(mhm_cfg)) call self%mhm%configure(mhm_cfg, self%exchange)
    if (present(mrm_cfg)) call self%mrm%configure(mrm_cfg, self%exchange)
    ! connect
    if (self%input%config%active) call self%input%connect()
    if (self%meteo%config%active) call self%meteo%connect()
    if (self%mpr%config%active) call self%mpr%connect()
    if (self%mhm%config%active) call self%mhm%connect()
    if (self%mrm%config%active) call self%mrm%connect()
  end subroutine domain_configure

  !> \brief Initialize the domain and do the initial state calculations in the components.
  subroutine domain_initialize(self, parameters)
    class(domain_t), intent(inout) :: self
    !> a set of global parameter (gamma) to run mHM, DIMENSION [no. of global_Parameters]
    real(dp), dimension(:), optional, intent(in) :: parameters
    call message(" ... initialize domain")
    call self%exchange%parameters%set(parameters)
    if (self%input%config%active) call self%input%initialize()
    if (self%meteo%config%active) call self%meteo%initialize()
    if (self%mpr%config%active) call self%mpr%initialize()
    if (self%mhm%config%active) call self%mhm%initialize()
    if (self%mrm%config%active) call self%mrm%initialize()
  end subroutine domain_initialize

  subroutine domain_update(self)
    class(domain_t), intent(inout) :: self
    call self%exchange%time%add(self%exchange%step)
    self%exchange%step_count = self%exchange%step_count + 1_i4
    if (self%input%config%active) call self%input%update()
    if (self%meteo%config%active) call self%meteo%update()
    if (self%mpr%config%active) call self%mpr%update()
    if (self%mhm%config%active) call self%mhm%update()
    if (self%mrm%config%active) call self%mrm%update()
    if (self%exchange%time%is_new_year()) &
      call message(" ... finished year: ", n2s(self%exchange%time%year - 1_i4))
  end subroutine domain_update

  subroutine domain_finalize(self)
    class(domain_t), intent(inout) :: self
    call message(" ... finalizing domain")
    if (self%mrm%config%active) call self%mrm%finalize()
  end subroutine domain_finalize

end module mo_domain
