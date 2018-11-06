Hands-On Exercise
=================

To compile the emulated co_sum test inside the virtual machine, execute 
the following steps at the command line (if you're reading the current
REAMDE.md file outside a browser, ignore the back ticks below:

```
cd code-jams
mkdir build
cd build
FC=caf cmake ..
make 
cafrun -n 4 ./test-emulated-co_sum
```

Your exercise is to replace the co_broadcast call in 
emulated-collectives-implementation.f90 with your own broadcast:

1. First try a sequential broadcast wherein the broadcast image
   puts the data on each receiving image one-by-one.
2. Next try writing a binary sum co_broadcast that effective
   reverses the direction of information information flow 
   employed in the emulated co_sum.

Add your emulated co_broadcast interface body in
emulated-collectives-interface.f90 and your emualated co_broadcast
subroutine definition in emulated-collectives-implementation.f90.
