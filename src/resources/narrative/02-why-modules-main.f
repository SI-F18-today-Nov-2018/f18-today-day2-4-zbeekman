! 03-why-modules-main.f
!
! Compiling and executing:
!
!    caf 03-why-modules-main.f 04-why-modules-external-subroutine.f90
!    cafrun -n 4 ./a.out
!
! Because the external subroutine "output" has an implicit interface,
! the compiler resorts to passing arguments by sequence association,
! simply apportioning the actual-argument bits sequentially to the
! dummy argument variables.
!
      program main
        implicit none
        integer num_vectors
        parameter(num_vectors=1)
        call output(this_image(),num_vectors)
      end program
