!> \file    mo_mhm_container.f90
!> \brief   \copybrief mo_mhm_container
!> \details \copydetails mo_mhm_container

!> \brief   Module for a mHM process container.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Aug 2025
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
!> \ingroup f_exchange
module mo_mhm_container
  use mo_namelists, only: &
    nml_project_description_t, &
    nml_mainconfig_t, &
    nml_lcover_t, &
    nml_directories_general_t, &
    nml_processselection_t, &
    nml_mainconfig_mhm_mrm_t, &
    nml_time_periods_t, &
    nml_optimization_t, &
    nml_optional_data_t, &
    nml_baseflow_config_t, &
    nml_panevapo_t, &
    nml_nloutputresults_t
  use mo_kind, only: i4

  !> \class   mhm_config_t
  !> \brief   Configuration for a single mHM process container.
  type, public :: mhm_config_t
    logical :: active = .false. !< flag to activate the mHM process container
    integer(i4) :: domain !< domain number to read correct configuration
    type(nml_project_description_t) :: project_description !< project_description configuration
    type(nml_mainconfig_t) :: mainconfig !< mainconfig configuration
    type(nml_lcover_t) :: lcover !< lcover configuration
    type(nml_directories_general_t) :: directories_general !< directories_general configuration
    type(nml_processselection_t) :: processselection !< processselection configuration
    type(nml_mainconfig_mhm_mrm_t) :: mainconfig_mhm_mrm !< mainconfig_mhm_mrm configuration
    type(nml_time_periods_t) :: time_periods !< time_periods configuration
    type(nml_optimization_t) :: optimization !< optimization configuration
    type(nml_optional_data_t) :: optional_data !< optional_data configuration
    type(nml_baseflow_config_t) :: baseflow_config !< baseflow_config configuration
    type(nml_panevapo_t) :: panevapo !< panevapo configuration
    type(nml_nloutputresults_t) :: nloutputresults !< nloutputresults configuration
  end type mhm_config_t

  !> \class   mhm_t
  !> \brief   Class for a single mHM process container.
  type, public :: mhm_t
    type(mhm_config_t) :: config !< configuration of the mHM process container
  contains
    procedure :: init => mhm_init
  end type mhm_t

contains

  !> \brief Initialize the mHM process container.
  subroutine mhm_init(self, config)
    class(mhm_t), intent(inout) :: self
    type(mhm_config_t), intent(in) :: config !< initialization config for mHM
    self%config = config
  end subroutine mhm_init

  !> \brief Initialize the mHM process container.
  subroutine mhm_config_read(self, file, domain)
    class(mhm_config_t), intent(inout) :: self
    character(*), intent(in) :: file !< file containing the namelists
    integer(i4), intent(in) :: domain !< domain number to read correct configuration
    self%active = .true.
    self%domain = domain
    call self%project_description%read(file)
    call self%mainconfig%read(file)
    call self%lcover%read(file)
    call self%directories_general%read(file)
    call self%processselection%read(file)
    call self%mainconfig_mhm_mrm%read(file)
    call self%time_periods%read(file)
    call self%optimization%read(file)
    call self%optional_data%read(file)
    call self%baseflow_config%read(file)
    call self%panevapo%read(file)
    call self%nloutputresults%read(file)
  end subroutine mhm_config_read

end module mo_mhm_container
