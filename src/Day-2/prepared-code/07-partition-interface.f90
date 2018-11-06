module partition_interface
  !! author: Damian Rouson
  !!
  !! Input global partition shape parameters from a file and distribute the partitions across
  !! images.
  implicit none

  private
  public :: partition

  type partition
    integer, allocatable :: global_grid_shape_(:)
    real, allocatable :: x(:,:,:,:)
  contains
    procedure input_from_file
    procedure allocate_my_partition
  end type

  interface

    module subroutine input_from_file(this,file_name)
      implicit none
      class(partition), intent(inout) :: this
      character(len=*), intent(in) :: file_name
    end subroutine

    module subroutine allocate_my_partition(this)
      implicit none
      class(partition), intent(out) :: this
    end subroutine

  end interface

end module
