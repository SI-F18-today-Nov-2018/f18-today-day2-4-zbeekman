program main
  !! author: Damian Rouson
  !!
  !! Demonstrate inheritance via type extension.  Demonstrae the use of generic type-bound
  !! operators on a class with a an allocatable coarray component.

   use assertion_interface, only : assert
   use structured_grid_interface, only : space_dimension
   use functional_grid_interface, only : functional_grid

   implicit none

   integer, allocatable :: nx(:)
   type(functional_grid) :: mesh

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
