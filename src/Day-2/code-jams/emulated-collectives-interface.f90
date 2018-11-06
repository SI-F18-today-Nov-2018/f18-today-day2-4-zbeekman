module emulated_collectives_interface
  !! author: Damian Rouson
  !!
  !! Emulate the Fortran 2018 collective subroutine co_sum
  implicit none

  private
  public :: emulated_co_sum

  interface
    module subroutine emulated_co_sum(a)
      implicit none
      integer, intent(inout) :: a
    end subroutine
  end interface
end module
