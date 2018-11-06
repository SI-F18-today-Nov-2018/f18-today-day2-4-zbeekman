C 00-why-fortran-shortest-program.f
C
C Compile & run 4 images with OpenCoarrays (www.opencoarrays.org):
C
C    caf 00-why-fortran-shortest-program.f
C    cafrun -n 4 ./a.out
C
C As of its 2008 standard, Fortran became a parallel programming
C language.  A standard-compliant Fortran compiler can generate a
C parallel executable program even from a serial Fortran 77 code,
C each image (replica) of which would, in the trivial case, perform
C the same instructions in parallel and asynchronously.  Even the
C shortest Fortran program is therefore a parallel program.
C
      end
