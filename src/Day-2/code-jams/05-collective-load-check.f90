module structured_grid_interface
  implicit none

  integer, parameter :: space_dimension=3

  interface

    module function get_grid_resolution(file_name) result(resolution)
      !! Read and return grid size parameters
      character(len=*), intent(in) :: file_name
      integer :: resolution(space_dimension)
    end function

    module subroutine allocate_my_partition(resolution)
      !! Establish a coarray storing grid locations
      integer, intent(in) :: resolution(space_dimension)
    end subroutine

    module function my_partition_size() result(my_num_points)
      !! Return the number of grid points owned by this image
      integer my_num_points
    end function

  end interface

end module

submodule(structured_grid_interface) structured_grid_implementation
  use assertion_interface, only : assert, assertions
  implicit none

  real, allocatable :: x(:,:,:,:)[:]
    !! grid point locations

contains

    module procedure get_grid_resolution

      integer, parameter :: success=0, reader=1
      integer nx, ny, nz, file_unit, stat
      integer n(space_dimension)
      namelist/grid_resolution/ nx, ny, nz

      if (this_image()==reader) then
        open(newunit=file_unit,file=file_name,iostat=stat)
        call assert(stat==success,"file opened sucessfully")

        read(unit=file_unit,nml=grid_resolution,iostat=stat)
        call assert(stat==success,"file read sucessfully")

        resolution = [nx,ny,nz]

      end if

      call co_broadcast(resolution,source_image=reader)

    end procedure

    module procedure allocate_my_partition

       integer alloc_result
       integer, parameter :: success=0

      associate(ni=>num_images())
      associate(nx=>(resolution(1)),ny=>(resolution(2)),nz=>(resolution(3)))

       if (assertions) then
         !! Requirements
         call assert(nx>=ni,"enough planes to distribute")
         call assert(mod(nx,ni)==0,"planes evenly divisible acrosss images")
       end if

        associate(my_yz_planes=>nx/num_images())

          allocate(x(my_yz_planes,ny,nz,space_dimension)[*],stat=alloc_result)

        end associate; end associate

        if (assertions) then
          !! Assurances
          call assert(alloc_result==success,"coarray allocation succeeded")
        end if
      end associate

    end procedure

    module procedure my_partition_size
      my_num_points = size(x)
    end procedure

end submodule

program main
  !! author: Damian Rouson
  !!
  !! Establish a coarray for distributing structured-grid vertex locations across images.
  !! Use collective subroutines to distribute the grid dimensions and verify load distribution

   use assertion_interface, only : assert
   use structured_grid_interface, only : get_grid_resolution, allocate_my_partition, my_partition_size, space_dimension

   implicit none

   integer, allocatable :: nx(:)

   nx = get_grid_resolution('grid-parameters.nml')
   call assert( size(nx)==space_dimension .and. all(nx>0),"acceptable 3D grid resolution")

   call allocate_my_partition(nx)

   block
     integer load
     load = my_partition_size()
     call co_sum( load )
     call assert( load == product(nx)*space_dimension, "all points distributed")
   end block

   sync all
   block
     integer, parameter :: reporter=1
     if (this_image()==reporter) print *,"Test passed."
   end block

end program
