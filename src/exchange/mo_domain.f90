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
  use mo_kind, only: i4
  use mo_message, only: error_message
  use mo_exchange_type, only: exchange_t
  use mo_string_utils, only: n2s=>num2str
  use mo_datetime, only: datetime
  ! containers
  use mo_input_container, only: input_t, input_config_t
  use mo_meteo_container, only: meteo_t, meteo_config_t
  use mo_mpr_container, only: mpr_t, mpr_config_t
  use mo_mhm_container, only: mhm_t, mhm_config_t
  use mo_mrm_container, only: mrm_t, mrm_config_t

  !> \class   domain_t
  !> \brief   Class for a single mHM domain.
  type, public :: domain_t
    type(exchange_t) :: exchange !< the exchange container with all exchanged variables for this domain
    type(input_t) :: input !< the input container providing inputs from file or couplers
    type(meteo_t) :: meteo !< the meteorology container providing processed meteorological data
    type(mpr_t) :: mpr !< the MPR container providing parameter fields for the process containers
    type(mhm_t) :: mhm !< the mHM process container calculating vertical hydrological processes
    type(mrm_t) :: mrm !< the mRM process container for routing related processes
  contains
    procedure :: init => domain_init
  end type domain_t

  !> \class   domain_list
  !> \brief   Class to hold a list of all domains as a linked list of pointers.
  type, extends(list) :: domain_list
    integer(i4) :: counter = 0_i4 !< internal counter for added domains
  contains
    procedure :: get_domain => domain_list_get
    procedure :: add_domain => domain_list_add
  end type

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
  subroutine domain_init(self, start_time, input_cfg, meteo_cfg, mpr_cfg, mhm_cfg, mrm_cfg)
    class(domain_t), intent(inout) :: self
    type(datetime), intent(in) :: start_time !< start time of the simulation
    type(input_config_t), intent(in), optional :: input_cfg !< configuration for the input container
    type(meteo_config_t), intent(in), optional :: meteo_cfg !< configuration for the meteo container
    type(mpr_config_t), intent(in), optional :: mpr_cfg !< configuration for the mpr container
    type(mhm_config_t), intent(in), optional :: mhm_cfg !< configuration for the mhm container
    type(mrm_config_t), intent(in), optional :: mrm_cfg !< configuration for the mrm container
    call self%exchange%init(start_time)
    if (present(input_cfg)) call self%input%init(input_cfg)
    if (present(meteo_cfg)) call self%meteo%init(meteo_cfg)
    if (present(mpr_cfg)) call self%mpr%init(mpr_cfg)
    if (present(mhm_cfg)) call self%mhm%init(mhm_cfg)
    if (present(mrm_cfg)) call self%mrm%init(mrm_cfg)
  end subroutine domain_init

end module mo_domain
