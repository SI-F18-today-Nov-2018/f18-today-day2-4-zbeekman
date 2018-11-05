program main
  !! author: Damian Rouson
  !!
  !! Gather all greetings on to image 1 and print
  use assertion_interface, only : assert, assertions
  implicit none
  integer, parameter :: max_greeting=128
  character(len=max_greeting), save :: greeting[*]
  associate(me=>this_image(),ni=>num_images())
    write(greeting,"(2(a,i4))") "Hello from image",me,"of",ni
    sync all
    if (me==1) then
      print *,greeting
      print_greeting: block
        integer image
        character(len=max_greeting) expected_greeting
        do concurrent(image=2:ni)
          print *,greeting[image]
          if (assertions) then
             write(expected_greeting,"(2(a,i4))") "Hello from image",image,"of",ni
             call assert(expected_greeting==greeting,"expected greeting received")
          end if
        end do
      end block print_greeting
    end if
  end associate
end program
