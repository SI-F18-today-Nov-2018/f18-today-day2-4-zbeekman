module emulated_collectives_interface
  !! Fortran 2008 coarray emulations of Fortran 2018 intrinsic collective subroutines
  implicit none

  interface co_sum
    module procedure co_sum_integer
  end interface

  interface co_broadcast
    module procedure co_broadcast_integer
  end interface

  interface

    module subroutine co_sum_integer(a,result_image,stat,errmsg)
      !! parallel computation of the sum of the first argument 
      implicit none
      integer, intent(inout) :: a
      integer, intent(in), optional :: result_image
      integer, intent(out), optional ::  stat
      character(len=*), intent(inout), optional :: errmsg
    end subroutine

    module subroutine co_broadcast_integer(a,source_image,stat,errmsg)
      !! parallel one-to-all communication of the value of first argument 
      implicit none
      integer, intent(inout) :: a
      integer, intent(in) :: source_image
      integer, intent(out), optional ::  stat
      character(len=*), intent(inout), optional :: errmsg
    end subroutine

  end interface

end module emulated_collectives_interface
