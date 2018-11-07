module structured_grid_interface
  implicit none

  private
  public :: structured_grid
  public :: space_dimension

  integer, parameter :: space_dimension=3

  type structured_grid
    real, allocatable :: x(:,:,:,:)[:]
      !! grid point locations
  contains
    procedure get_grid_resolution
    procedure allocate_my_partition
    procedure my_partition_size
  end type

  interface

    module function get_grid_resolution(this,file_name) result(resolution)
      !! Read and return grid size parameters
      class(structured_grid), intent(in) :: this
      character(len=*), intent(in) :: file_name
      integer :: resolution(space_dimension)
    end function

    module subroutine allocate_my_partition(this,resolution)
      !! Establish a coarray storing grid locations
      class(structured_grid), intent(inout) :: this
      integer, intent(in) :: resolution(space_dimension)
    end subroutine

    module function my_partition_size(this) result(my_num_points)
      !! Return the number of grid points owned by this image
      class(structured_grid), intent(in) :: this
      integer my_num_points
    end function

  end interface

end module

submodule(structured_grid_interface) structured_grid_implementation
  use assertion_interface, only : assert, assertions
  implicit none

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

       integer alloc_result, image
       integer, parameter :: success=0

      associate(ni=>num_images(), me=>this_image())
      associate(nx=>(resolution(1)),ny=>(resolution(2)),nz=>(resolution(3)))

       if (assertions) then
         !! Requirements
         call assert(nx>=ni,"enough planes to distribute")
       end if

        associate(num_ny_planes=>(nx))
        associate(quotient=>nx/num_images())
        associate(remainder=>mod(nx,num_images()))

        associate(my_first=>1+sum([(quotient+overflow(image,remainder),image=1,me-1)]))
        associate(my_last=>my_first+quotient+overflow(me,remainder)-1)

          allocate(this%x(my_first:my_last,ny,nz,space_dimension)[*],stat=alloc_result)

        end associate; end associate; end associate; end associate; end associate; end associate

        if (assertions) then
          !! Assurances
          call assert(alloc_result==success,"coarray allocation succeeded")
        end if
      end associate

    contains
      pure function overflow(image_number,remainder) result(extra)
        integer, intent(in) :: image_number, remainder
        integer :: extra
        extra = merge(1,0,image_number<=remainder)
      end function

    end procedure

    module procedure my_partition_size
      my_num_points = size(this%x)
    end procedure

end submodule

program main
  !! author: Damian Rouson
  !!
  !! Establish a coarray for distributing structured-grid vertex locations across images.
  !! Use collective subroutines to distribute the grid dimensions and verify load distribution

   use assertion_interface, only : assert
   use structured_grid_interface, only : structured_grid, space_dimension

   implicit none

   integer, allocatable :: nx(:)
   type(structured_grid) :: mesh

   nx = mesh%get_grid_resolution('grid-parameters.nml')
   call assert( size(nx)==space_dimension .and. all(nx>0),"acceptable 3D grid resolution")

   call mesh%allocate_my_partition(nx)

   block
     integer load
     load = mesh%my_partition_size()
     call co_sum( load )
     call assert( load == product(nx)*space_dimension, "all points distributed")
   end block


   sync all
   block
     integer, parameter :: reporter=1
     if (this_image()==reporter) print *,"Test passed."
   end block

end program
