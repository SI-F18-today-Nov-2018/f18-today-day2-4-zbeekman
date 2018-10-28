! 02-hello-exoplanet.f90
!
! Compiling and executing:
!
!    caf 02-hello-exoplanet.f90
!    cafrun -n 4 ./a.out
!
! Coarrays provide a mechanism for distributing and communicating data between images.
! Synchronization mechanisms order segements of executing statements.
!
program hello_exoplanet
  implicit none
  character(len=128) greeting[*]                              ! A scalar coarray
  associate( me=>this_image(), num_planets=>num_images() )    ! Associate a name with an expression ("runtime constant")
    write(greeting,*) "Hello from planet",me,"of",num_planets ! Internal file I/O on local data (implicit coindex: me)
    sync all                                                  ! Barrier
    print_greetings: block                                    ! Local scope for declarations
      integer planet
      if (me==1) then
        do concurrent(planet=1:num_planets)                   ! Indicate lack of data dependencies between iterations
          print *,greeting[planet]                            ! Image 1 grabs and prints eveyone's greeting
        end do
      end if
    end block print_greetings
  end associate
end program
