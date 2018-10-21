! 05-why-modules-explicit-interface.f90
!
! Attempt to compile (fails due to type/rank mismatch):
! 
!    caf 05-why-modules-explicit-interface.f90 06-why-modules-use-module.f90
!
! Q: Why use modules?
! A: Among other benefits, modules provide explicit interfaces, which ensure 
!    link-time checking of the consistency of type, kind, and rank of procedure 
!    arguments.
!
module vectors_module
  implicit none
contains
  include "04-why-modules-external-subroutine.f90"
end module
