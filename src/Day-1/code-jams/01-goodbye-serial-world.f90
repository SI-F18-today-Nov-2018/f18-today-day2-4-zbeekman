program main
  !! author: Damian Rouson
  !! date: 11/05/2018
  !!
  !! Demonstrate name-space enumeration and asynchronous execution in Fortran 2018
  implicit none
  print *,"Goodbye serial world from image ",this_image()," of ",num_images()
end program
