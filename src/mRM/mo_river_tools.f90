!> \file    mo_river_tools.f90
!> \copydoc mo_river_tools

!> \brief   River related tools.
!> \details This module contains tools to deal with river related data.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    May 2025
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! This code is released under the LGPLv3+ license \license_note
module mo_river_tools

  use mo_kind, only: dp, i4
  use mo_os, only: path_ext
  use mo_io, only: loadtxt
  use mo_netcdf, only : NcDataset, NcVariable
  use mo_message, only: error_message
  use mo_string_utils, only : splitString
  use mo_grid, only: is_x_axis, is_lon_coord, is_y_axis, is_lat_coord

  implicit none
  private
  public :: read_scc_gauges

contains

  !> \brief Read scc gauges specifications
  !> \details Read scc gauges specifications
  subroutine read_scc_gauges(file, scc_gauges, scc_latlon, scc_id)
    character(len=*), intent(in) :: file !< file containing the scc-gauges specification (.nc will be read as NetCDF)
    real(dp), allocatable, intent(out) :: scc_gauges(:,:) !< SCC gauge location coordinates (dim 1: id, dim 2: (x,y))
    logical, optional, intent(out) :: scc_latlon !< indicator for geographical coordinates of gauges
    integer(i4), allocatable, optional, intent(out) :: scc_id(:) !< list of scc gauge IDs
    type(NcDataset) :: nc
    type(NcVariable) :: x_var, y_var, scc_var, c_var
    character(len=256) :: tmp_str
    character(len=256), allocatable, dimension(:) :: coords_str
    logical :: is_lon, is_lat
    real(dp), allocatable :: x(:), y(:)
    integer(i4) :: i

    if (path_ext(file) /= ".nc") then
      ! everything without .nc extension is read as CSV file
      call loadtxt(file, scc_gauges)
      if (present(scc_latlon)) scc_latlon = .true. ! assume latlon coordinates
      if (present(scc_id)) scc_id = [(i, i=1_i4,size(scc_gauges,dim=1))] ! default numbering
      return
    end if

    nc = NcDataset(file, "r")
    if (.not.nc%hasVariable("station")) then
      call error_message("read_scc_gauges: NetCDF file has no variable 'station' - ", file)
    end if

    scc_var = nc%getVariable("station")
    if (.not.scc_var%hasAttribute("coordinates")) then
      call error_message("read_scc_gauges: NetCDF variable 'station' has no attribute coordinates - ", file)
    end if

    call scc_var%getAttribute("coordinates", tmp_str)
    coords_str = splitString(trim(tmp_str), " ")
    if (size(coords_str) /= 2) then
      call error_message("read_scc_gauges: NetCDF variable 'station' needs to have exactly two coordinates - ", file)
    end if

    ! determine coordinates order
    c_var = nc%getVariable(trim(coords_str(1)))
    if (is_x_axis(c_var).or.is_lon_coord(c_var)) then
      x_var = nc%getVariable(trim(coords_str(1)))
      y_var = nc%getVariable(trim(coords_str(2)))
    else
      x_var = nc%getVariable(trim(coords_str(2)))
      y_var = nc%getVariable(trim(coords_str(1)))
    end if

    is_lon = is_lon_coord(x_var)
    is_lat = is_lat_coord(y_var)
    if (.not.(is_x_axis(x_var).or.is_lon)) then
      call error_message("read_scc_gauges: NetCDF variable '", trim(x_var%getName()) ,"' is not a x-coordinate - ", file)
    end if
    if (.not.(is_y_axis(y_var).or.is_lat)) then
      call error_message("read_scc_gauges: NetCDF variable '", trim(y_var%getName()) ,"' is not a y-coordinate - ", file)
    end if
    if (is_lon.neqv.is_lat) then
      call error_message("read_scc_gauges: NetCDF 'station' coordinates have different projections - ", file)
    end if
    if (present(scc_latlon)) scc_latlon = is_lon

    call x_var%getData(x)
    call y_var%getData(y)
    if (size(x) /= size(y)) then
      call error_message("read_scc_gauges: NetCDF 'station' coordinates have different sizes - ", file)
    end if
    allocate(scc_gauges(size(x), 2))
    scc_gauges(:, 1) = x(:)
    scc_gauges(:, 2) = y(:)
    if (present(scc_id)) call scc_var%getData(scc_id)

  end subroutine read_scc_gauges

end module mo_river_tools
