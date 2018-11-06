! 08-why-use-derived-types.f90
!
! Compile and execute:
!
!    caf 08-why-use-derived-types.f90
!    cafrun -np 3 ./a.out # Image 1 handles two vectors; others handle 1.
!    cafrun -np 4 ./a.out # Even distribution of vectors.
!    cafrun -np 5 ./a.out # Zero-sized array prints as blank line.
!
! Encapsulating data inside a derived type facilitates satisfying the requirement
! on matching the encapsulating coarray bounds and cobounds while allowing for
! allocating derived-type components with different bounds and cobounds.  Care
! must be taken to ensure that the (asynchronus) component allocations happen
! before accessing the components.
!
program main
  implicit none

  type vectors
    real, allocatable :: x(:,:)
  end type

  integer, parameter :: num_vectors=4

  associate( me => this_image(), nimages=>num_images() )
  ! Block distribution of vectors: unit increment of my_num_vectors for image number <= remainder
  associate( my_num_vectors => num_vectors/nimages + merge(1,0,me<=mod(num_vectors,nimages)) )

    block
      use vectors_module, only : vectors,output
      type(vectors), save :: positions[*]

      asynchronous_allocation: block
        integer :: ij
        integer, parameter :: space_dimensions=3
        allocate(positions%x(my_num_vectors,space_dimensions),source = real(me) )

!       Alternative:
!       positions%x = real( reshape( [(me,ij=1,my_num_vectors*space_dimensions)] , [my_num_vectors,space_dimensions] ) )

        print *,positions%x
      end block asynchronous_allocation

      associate(left=>merge(me-1,nimages,me/=1),right=>merge(me+1,1,me/=nimages))
        select case(nimages)
          case(1)
          case(2)
            associate(neighbor=>merge(2,1,me==1))
              sync images(neighbor)
            end associate
          case default
              sync images([left,right])
        end select
        if (me>1 .and. me<4) then
            if ( any(positions[left]%x  /= real(left) ) ) error stop "invalid left neighbor"
            if ( any(positions[right]%x /= real(right)) ) error stop "invalid right neighbor"
        end if
      end associate

      sync all
      if (me==1) print *,"Test passed"
    end block
  end associate
  end associate

end program
