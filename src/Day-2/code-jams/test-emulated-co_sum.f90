program main
  !! author: Damian Rouson
  !!
  !! Verify the collective sum result on all images
  use assertion_interface, only : assert
  use emulated_collectives_interface, only : emulated_co_sum
  implicit none
  integer :: image_number, i
  image_number = this_image()
  call emulated_co_sum(image_number)
    !! Sum all image numbers
  call assert( image_number==sum( [ (i,i=1,num_images()) ] ),"correct image-number tally")
  sync all
    !! Wait for every image to get past the assertion
  if (this_image()==1) print *,"Test passed."
end program
