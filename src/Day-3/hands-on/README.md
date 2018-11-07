Hands-on Exercise
=================

This directory provides a test to drive your development of an
aysmmetrically distributed set of grid points.  We wil employ
test-driven development, an iterative processs that proceeds
as described below.

Suggested Workflow
------------------
1. Try building the test with `cmake`:
```
  mkdir build
  cd build
  FC=caf cmake ..
  make
```
2. Guided by the `cmake` errors,  create empty versions of the missing file(s).
3. Try building.
3. Guided by compiler errors, create the missing module, submodule, types, etc.
4. Try building.
5. Use the comments in the main program [test-asymmetric-load-check.f90] to
   guide you toward reusing code from the related Day-2/code-jam.
6. The resulting code will work when the number of y-z planes images can be
   evenly distributed across images.

Extra Credit
------------
Try setting `nx=27` in `grid-parameters.nml` and running with `cafrun -n 4`.
You might handle the resulting uneven distribution of planes by adding one extra 
plane to each image for images 1 through the image number that equals the remainder 
in the integer division `num_images()/nx` where `nx` equals `size(this%x,1)`.

Solution
--------
The ../solution directory demonstrates how to handle uneven load balancing by the
approach recommended in the [Extra Credit]. 


[test-asymmetric-load-check.f90]: https://github.com/sourceryinstitute/si-nov-2018/blob/master/src/Day-3/hands-on/test-asymmetric-load-check.f90
[Extra Credit]: #extra-credit
