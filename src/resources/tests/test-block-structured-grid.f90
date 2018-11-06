program main
  !! author: Damian Rouson
  !! date: 10/29/2018
  !!
  !! Test cartesian_grid definition
  use block_structured_grid_interface, only : block_structured_grid
  implicit none
  type(block_structured_grid) :: mesh
  associate(me=>this_image())
    if (me==1) call mesh%input_from_file("blocks-definition.nml")
    sync all
    if (me==1) print *,"Test passed."
  end associate
end program
