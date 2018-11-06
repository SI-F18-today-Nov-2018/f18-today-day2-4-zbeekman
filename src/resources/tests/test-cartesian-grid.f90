program main
  !! author: Damian Rouson
  !! date: 10/29/2018
  !!
  !! Test cartesian_grid definition
  use cartesian_grid_interface, only : cartesian_grid
  implicit none
  type(cartesian_grid) :: mesh
  print *,"Test passed."
end program
