program main
  !! author: Damian Rouson
  !! date: 11/04/2018
  !!
  !! Demonstrate asynchronous, unordered execution:
  !!
  !!    caf 01-goodbye-serial-world.f90
  !!    cafrun -n 4 ./a.out

  implicit none
  print *,"Goodbye, serial world from image ",this_image()," of ",num_images()
end program
