! 10-why-use-object-oriented-programming.F90
!
! Compile and execute:
!
!    caf 10-why-use-object-oriented-programming.F90
!    cafrun -np 3 ./a.out # Image 1 handles two vectors; others handle 1.
!    cafrun -np 4 ./a.out # Even distribution of vectors.
!    cafrun -np 5 ./a.out # Zero-sized array prints as blank line.
!
! Object-oriented programming facilitates code reuse via type extension.  Extended types
! inherit state (components) and behavior (type-bound procedures), obviating the need to 
! re-implement them for each related type.
!
module vectors_module
  use iso_fortran_env, only : real64  ! 64-bit kind parameter (Fortran 2008)
  implicit none

  private
  public :: vectors

  type vectors
    private
    real(real64), allocatable :: x(:,:) ! new internal representation
  contains
    procedure :: set
    procedure :: output
  end type

contains

   ! Data privacy ensures there are no direct references to x outside vectors_module.  In
   ! particular, no references to x appear in any of the dummy arguments named in vectors_module
   ! procedures.  This implies that revising the kind of x does not cause corresponding 
   ! reivisions of the kinds of the associated actual arguments that appear wherever a program 
   ! line calls a vectors_module procedure.  

   subroutine set(this,components)
     class(vectors) :: this
     real :: components(:,:)
     this%x = components
   end subroutine

   subroutine output(this)
     class(vectors) :: this
     if (.not. allocated(this%x)) error stop "vectors%output: unallocated x component"
     print *,this%x
   end subroutine

end module

module cartesian_vectors_module
  use vectors_module, only : vectors
  implicit none
  type, extends(vectors) :: cartesian_vectors
  end type
end module


! Main program unmodified from 08-why-use-derived-types.F90
program main
  implicit none
  integer, parameter :: num_vectors=4

  associate( me => this_image(), nimages=>num_images() )
  ! Block distribution of vectors: unit increment of my_num_vectors for image number <= remainder
  associate( my_num_vectors => num_vectors/nimages + merge(1,0,me<=mod(num_vectors,nimages)) )

    block
      use cartesian_vectors_module, only : cartesian_vectors
      type(cartesian_vectors), save :: positions[*]

      asynchronous_allocation: block
        integer :: ij
        integer, parameter :: space_dimensions=3
        call &
          positions%set(components=real(reshape([(me,ij=1,my_num_vectors*space_dimensions)],[my_num_vectors,space_dimensions])))
      end block asynchronous_allocation

#ifdef ACCESS_COMPONENT_OF_COINDEXED_VARIABLE
      associate(neighbor=>merge(me+1,1,me/=nimages))
        sync images(neighbor)             ! Ensure my neighbor has set its positions object
        call positions[neighbor]%output() ! Print my neighbor's component
      end associate
#else
        call positions%output() ! Print my component (Gfortran 6 doesn't support accessing component of coindexed variable)
#endif
    end block
  end associate
  end associate

end program
