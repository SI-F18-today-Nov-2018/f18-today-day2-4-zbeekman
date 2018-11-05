module block_structured_grid_interface
  !! author: Damian Rouson
  !! date: 10/29/2018
  !!
  !! Define the grid block structure and global domain
  use cartesian_grid_interface, only : cartesian_grid
  implicit none

  private
  public :: block_structured_grid

  type block_structured_grid
    private
    integer, allocatable :: global_block_shape_(:)
    type(cartesian_grid), allocatable :: vertices(:)
  contains
    procedure :: input_from_file
    procedure :: partition
  end type

  interface

    module subroutine input_from_file(this,file_name)
      implicit none
      class(block_structured_grid), intent(out) :: this
      character(len=*), intent(in) :: file_name
    end subroutine

    pure module subroutine partition(this)
      implicit none
      class(block_structured_grid), intent(inout) :: this
    end subroutine

  end interface

end module block_structured_grid_interface
