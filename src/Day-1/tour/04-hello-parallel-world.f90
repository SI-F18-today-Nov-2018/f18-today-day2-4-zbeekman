program main
  !! author: Damian Rouson
  !! date: 11/4/2018
  !!
  !! Print greetings that image 1 gets from each other image
  !! To compile and execute,
  !!
  !!    caf 02-assertions-interface.f90 03-assertions-implementation.f90 04-hello-parallel-world.f90
  !!    cafrun -n 4 ./a.out
  !!
  use assertions_interface, only : assert,assertions
  integer, parameter :: test_reporter=1, max_string=100
  character(len=max_string) greeting[*],message_expected
  associate(me=>this_image(),ni=>num_images())
    write(greeting,"(2(a,i2))") "Hello from image",this_image(),"of",num_images()
    sync all
    if (me==test_reporter) then
      do concurrent(image=1:ni)
        print *,greeting[image]
        write(message_expected,"(2(a,i2))") "Hello from image",image,"of",ni
        if (assertions) call assert(message_expected==greeting[image],"received expected message",greeting[image])
      end do
      print *,"Test passed."
    end if
  end associate
end program
