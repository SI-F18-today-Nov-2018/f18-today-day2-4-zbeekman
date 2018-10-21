! 07-implicit-synchronization.f90
!
! Compile and execute:
!
! caf 07-implicit-synchronization.f90
! cafrun -np 4 ./a.out
!
! A coarray forms a partitioned global address space (PGAS).  To support one
! image efficiently accessing a second image's local data without the second
! image's involvement, allocation of the coarray must imply synchronization.
!
module vectors_module
  implicit none
contains
   ! A subprogram that does communicate between images may associate a
   ! a coarray actual argument with a non-coarray (local) dummy argument
   subroutine output(y)
      real, intent(in) :: y(:,:)
      print *,y
   end subroutine
end module

program main
  use vectors_module, only : output
  implicit none
  integer, parameter :: num_particles=4,space_dimensions=3
  real, allocatable :: x(:,:)[:]

  associate( me=>this_image(), images => num_images()  )

    ! All images must allocate coarrays with the same bounds and cobounds
    if (mod(num_particles,images)/=0) error stop "uneven distribution"

    associate( my_num_particles => num_particles/images)
      ! A coarray allocation requires is an implicit synchronization point
      allocate(x(my_num_particles,space_dimensions)[*],source=real(me))
      call output(x)
    end associate

    associate( neighbor => merge(me+1,1,me<images) )
      if ( any(x(:,:)[neighbor] /= real(neighbor) ))  error stop "incorrect neighbor"
    end associate

    sync all
    if (me==1) print *,"Test passed"

  end associate

end program
