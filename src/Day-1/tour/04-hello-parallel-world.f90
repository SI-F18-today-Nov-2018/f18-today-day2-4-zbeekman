program main
  use assertions_interface, only : assert,assertions
  integer, parameter :: test_reporter=1
  associate(me=>this_image())
    if (assertions) call assert(.false.,"tautology")
    sync all
    if (me==test_reporter) print *,"Test passed."
  end associate
end program
