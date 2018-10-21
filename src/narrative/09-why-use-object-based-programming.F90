! 09-why-use-object-based-programming.F90
!
! Compile and execute:
!
!    caf 09-why-use-object-based-programming.F90
!    cafrun -np 3 ./a.out # Image 1 handles two vectors; others handle 1.
!    cafrun -np 4 ./a.out # Even distribution of vectors.
!    cafrun -np 5 ./a.out # Zero-sized array prints as blank line.
!
! Object-based programming limits dependencies between scoping units by hiding encapsulated
! data and provides access to the encapsulated data through public procedures.  This prevents
! changes inside one scope form impacting code in other scopes as long as the interfaces
! of the public procedures used by the second scope remain unchanged.
!
module vectors_module
  use iso_fortran_env, only : real64  ! 64-bit kind parameter (Fortran 2008)
  implicit none

  private
  public :: vectors
  public :: set
  public :: output

  type vectors
    private
    real(real64), allocatable :: x(:,:) ! new internal representation
  end type

contains

   ! Data privacy ensures there are no direct references to x outside vectors_module.  In
   ! particular, no references to x appear in any of the dummy arguments named in vectors_module
   ! procedures.  This implies that revising the kind of x does not cause corresponding 
   ! reivisions of the kinds of the associated actual arguments that appear wherever a program 
   ! line calls a vectors_module procedure.  

   subroutine set(this,components)
     type(vectors) :: this
     real :: components(:,:)
     this%x = components
   end subroutine

   subroutine output(this)
     type(vectors) :: this
     if (.not. allocated(this%x)) error stop "vectors%output: unallocated x component"
     print *,this%x
   end subroutine

end module


! Main program unmodified from 08-why-use-derived-types.F90
program main
  implicit none
  integer, parameter :: num_vectors=4

  associate( me => this_image(), nimages=>num_images() )
  ! Block distribution of vectors: unit increment of my_num_vectors for image number <= remainder
  associate( my_num_vectors => num_vectors/nimages + merge(1,0,me<=mod(num_vectors,nimages)) )

    block
      use vectors_module, only : vectors,output,set
      type(vectors), save :: positions[*]

      asynchronous_allocation: block
        integer :: ij
        integer, parameter :: space_dimensions=3
        call &
          set(positions,components=real(reshape([(me,ij=1,my_num_vectors*space_dimensions)],[my_num_vectors,space_dimensions])))
      end block asynchronous_allocation

#ifdef ACCESS_COMPONENT_OF_COINDEXED_VARIABLE
      associate(neighbor=>merge(me+1,1,me/=nimages))
        sync images(neighbor)            ! Ensure my neighbor has set its positions object
        call output(positions[neighbor]) ! Print my neighbor's component
      end associate
#else
        call output(positions) ! Print my component (Gfortran 6 doesn't support accessing component of coindexed variable)
#endif
    end block
  end associate
  end associate

end program
