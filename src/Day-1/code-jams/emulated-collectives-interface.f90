module emulated_collectives_interface
  implicit none

  interface
     module subroutine co_sum(a)
       integer, intent(inout) :: a
     end subroutine
  end interface

end module
