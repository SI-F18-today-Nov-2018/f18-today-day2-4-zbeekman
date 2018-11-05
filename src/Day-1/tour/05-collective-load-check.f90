program main
  !! author: Damian Rouson
  !!
  !! Establish a coarray for distributing structured-grid vertex locations across images.
  !! Use collective subroutines to distribute the grid dimensions and verify load distribution

   use assertions_interface, only : assert, assertions
   implicit none

  real, allocatable :: x(:,:,:,:)[:]
    !! structured-grid vertex locations
  integer nx, ny, nz
    !! grid resolution in each of three spatial directions
  integer, parameter :: space_dimension=3

  namelist/grid_resolution/ nx, ny, nz

  associate(me=>this_image(),ni=>num_images())
    block
      integer file_unit, stat, load
      integer :: n(space_dimension)
      integer, parameter :: reader=1, success=0

      if (me==reader) then
        open(newunit=file_unit,file='grid.nml',status='old',iostat=stat)
        call assert(stat==success,"file opened successfully")

        read(unit=file_unit,nml=grid_resolution,iostat=stat)
        call assert(stat==success,"file read successfully")
      end if

      n=[nx,ny,nz]
      call co_broadcast(n,source_image=reader)
      nx=n(1); ny=n(2); nz=n(3)

      associate(num_yz_planes=>(nx))
        if (assertions) call assert(mod(num_yz_planes,ni)==0,"evenly divisible planes")
        associate(my_num_planes=>num_yz_planes/ni)
        allocate(x(my_num_planes,ny,nz,space_dimension)[*])
      end associate; end associate

      load = product(shape(x))
      call co_sum( load )
      call assert( load == nx*ny*nz*space_dimension, "all points distributed")

      close(file_unit)
    end block

    sync all
    if (me==reader) print *,"Test passed."
  end associate


end program
