module assertions_interface
  !! author: Damian Rouson
  !! date: 11/4/2018
  !!
  !! Facilitate runtime checking of assertions expected to always be true in normal execution
  implicit none
  private
  public :: assert, assertions

  logical, parameter :: assertions=.true.
    !! Conditioning assetions on a logical constant facilitates optimizing away all assertions
    !! via dead-code removal when the constant is set to .false.

  interface
    !! Define procedure interface bodies that external code may use without depending
    !! on the corresponding procedure definitions
    pure module subroutine assert(assertion,description,diagnostic_data)
      !! Initiate error termination if the passed logical expression evaluates to .false.
      implicit none
        !! "implicit none" at the top of the module does not penetrate into the "interface/end interface" block
        logical, intent(in) :: assertion
        character(len=*), intent(in) :: description
        class(*), intent(in), optional :: diagnostic_data
    end subroutine
  end interface

end module
