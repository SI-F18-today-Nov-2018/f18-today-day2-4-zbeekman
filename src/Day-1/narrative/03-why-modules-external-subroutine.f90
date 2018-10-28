! 04-why-modules-external-subroutine.f90
!
! Compiling and executing:
!
!    caf 03-why-modules-main.f90 04-why-modules-external-subroutine.f90
!    cafrun -n 4 ./a.out
!
! This subroutine is in a separate file from the main program in order to demonstrate
! that the compiler misses the inconsistencies between the type and rank of the actual
! argument "this_image()" in the main program and those of the dummy argument "x" in
! this subroutine.  (The GNU Fortran compiler catches the inconsistency when the
! subroutine and main program are in the same file even though the Fortran standard
! does not require it to be that smart.)

subroutine output(vector,num_vectors)
  implicit none
  integer spacedimension,num_vectors,i,j
  parameter(spacedimension=3)    ! Compile-time constant (Fortran 77)
  real vector(spacedimension,*)  ! Assumed-size array (Fortran 90)
  do 10 i=1,num_vectors
    print *,"Real vector on image",this_image(),":",(vector(i,j),j=1,spacedimension) ! Implied-do loop (Fortran 77)
  10 continue
  print *,"Component 1 as an integer:",transfer(vector(1,1),int(0)) ! Print the bits as if they form an integer (Fortran 90)
end subroutine
