program test_mrm

  use mo_kind, only: i4, i8, dp
  use mo_grid, only: grid_t
  use mo_grid_io, only: var, input_dataset
  use mo_river, only: river_t
  use mo_river_router, only: routing_steps
  use mo_river_upscaler, only: river_upscaler_t
  use mo_river_tools, only: read_scc_gauges
  use mo_string_utils, only: num2str
  use mo_os, only: path_ext, path_isfile
  use mo_message, only: error_message
  use mo_utils, only: locate

  implicit none

  type(river_t), target :: river, criver
  type(river_upscaler_t) :: upscaler
  type(grid_t), target :: grid, cgrid
  type(input_dataset) :: ds
  character(:), allocatable :: file, dem_file, slope_file
  integer(i4), allocatable :: mfdir(:,:), fdir(:)
  real(dp), allocatable :: mdem(:,:), dem(:), mslope(:,:), slope(:), scc_gauges(:,:)
  integer(i4) :: itime
  integer(i8) :: i, j
  logical :: scc_latlon

  file = "src/tests/files/fdir.asc"
  dem_file = "src/tests/files/dem.asc"
  slope_file = "src/tests/files/slope.asc"
  print*, "read data: ", file

  select case(path_ext(file))
    case(".nc")
      call ds%init(path=file, grid=grid, vars=[var(name="fdir", static=.true.)], grid_init_var="fdir")
      allocate(fdir(grid%ncells))
      call ds%read("fdir", fdir)
      call ds%close()
    case(".asc")
      call grid%from_ascii_file(file)
      call grid%read_data(file, mfdir)
      fdir = grid%pack(mfdir)
      deallocate(mfdir)
    case default
      call error_message("unknown file extension: ", path_ext(file))
  end select

  if (path_isfile(dem_file)) then
    print*, "read dem: ", dem_file
    call grid%read_data(dem_file, mdem)
    dem = grid%pack(mdem)
    deallocate(mdem)
  end if
  if (path_isfile(slope_file)) then
    print*, "read slope: ", slope_file
    call grid%read_data(slope_file, mslope)
    slope = grid%pack(mslope)
    deallocate(mslope)
  end if

  print*, "create river network:", grid%ncells
  call river%from_fdir(fdir, grid)
  call river%calc_slope(dem)
  deallocate(fdir)

  print*, "order network"
  call river%calc_order(root=.true.)
  print*, " ... n levels (longest river): ", river%order%n_levels

  print*, "calculate facc"
  call river%calc_facc()

  print*, "calculate upstream area"
  call river%calc_upstream_area()

!   print*, "upscale river"
!   cgrid = grid%derive_grid(upscaling_factor=10)
!   call upscaler%init(river, criver, cgrid, scc_gauges, scc_latlon)

!   print*, "calculate celerity"
!   call upscaler%calc_celerity(gamma=30.0_dp, slope=slope)

  print*, "write restart file to: river_restart.nc"
  call river%write_restart("river_restart.nc")

!   call criver%calc_facc()
!   if (.not.criver%scc) call criver%export("criver.nc")

    
end program test_mrm