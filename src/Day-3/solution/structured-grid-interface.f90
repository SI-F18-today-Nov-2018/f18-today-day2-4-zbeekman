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
