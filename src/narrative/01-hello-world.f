C 01-hello-world.f
C
C Compiling and executing with OpenCoarrays:
C
C    caf 01-hello-world.f
C    cafrun -n 4 ./a.out
C
C In a slightly more interesting case, each image can use Fortran
C 2008 intrinsic functions to print a unique image identifier and
C the total number of images.
C
      program hello_world
        implicit none
        write (*,*)"Hello from image",this_image(),"of",num_images()
      end program
