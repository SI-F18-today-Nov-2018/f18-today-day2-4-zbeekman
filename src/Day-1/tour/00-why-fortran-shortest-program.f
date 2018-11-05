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
C each instance ("image") of executes which "has its own execution
C state, floating-point status, and set of data objects,  input/output
C units, and procedure pointers." [Fortran 2018 standard]  The shortest
C Fortran 77 program is therefore a parallel Fortran 2018 program:
C
      end
