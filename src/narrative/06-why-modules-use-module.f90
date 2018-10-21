! 06-why-modules-use-module.f90 
!
! Demonstrate compilation error:
!
!   caf 05-why-modules-explicit-interface.f90 06-why-modules-use-module.f90
! 
program main
  use vectors_module, only : output
  implicit none
  integer num_vectors
  parameter(num_vectors=1)
  call output(real(this_image()),num_vectors)
end program
