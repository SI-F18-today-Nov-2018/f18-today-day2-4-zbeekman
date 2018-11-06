program main
  !! author: Damian Rouson
  !!
  !! Establish a coarray for distributing structured-grid vertex locations across images.
  !! Use collective subroutines to distribute the grid dimensions and verify load distribution

   use assertions_interface, only : assert, assertions
   use partition_interface, only : partition
   implicit none

  type(partition) ::  my_partition[*]
    !! structured-grid vertex locations
  integer nx, ny, nz
    !! grid resolution in each of three spatial directions
  integer, parameter :: space_dimension=3, reader=1

  namelist/grid_resolution/ nx, ny, nz

  associate(me=>this_image(),ni=>num_images())
    call my_partition%input_from_file('grid.nml')
    call my_partition%allocate_my_partition()

    sync all
    if (me==reader) print *,"Test passed."
  end associate


end program
