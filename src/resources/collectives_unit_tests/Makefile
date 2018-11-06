# Choose desired platform (options: virtual, cees):
machine=virtual
# Choose desired compiler (options: gnu, intel, cray, tau_intel):
compiler=gnu

executable=collectives_test
project_root=../../
collectives_path=$(project_root)/library

ifeq ($(compiler),gnu)
  lib=-lcaf_mpi
  compile=mpif90 -fcoarray=lib
  ifeq ($(machine),virtual)
    libpath=-L /opt/opencoarrays/lib
  else
    ifeq ($(machine),cees)
      libpath=-L /opt/opencoarrays-src/mpi
    endif
  endif
else
  ifeq ($(compiler),intel)
    compile=ifort -standard-semantics -O3 
  else
    ifeq ($(compiler),cray)
      compile=ftn -ew 
    else
      ifeq ($(compiler),tau_intel)
        compile=tauf90 -tau:serial,icpc,pdt  -standard-semantics -O3 -DTAU=1
      else
        ifeq ($(compiler),tau_aciss)
          compile=tau_f90.sh -standard-semantics -O3
        else
          ifeq ($(compiler),tau_cray)
            compile=tau_f90.sh -optCompInst -ew 
          else
            ifeq ($(compiler),pg)
              compile=pgfortran -Mallocatable=03
            else
              ifeq ($(compiler),ibm)
                compile=xlf2003
              else
                ifeq ($(compiler),nag)
                  compile=nagfor -f2008 -fpp -C=all
                endif
              endif
            endif
          endif
        endif
      endif
    endif
  endif
endif

$(executable): collectives_test.o collectives.o Makefile
	$(compile) -fcoarray=lib $(libpath) collectives_test.o collectives.o $(objects) -o $(executable) $(lib)

collectives_test.o: collectives_test.f90  collectives.o Makefile
	$(compile) -fcoarray=lib -c collectives_test.f90

collectives.o: $(collectives_path)/collectives.f90  Makefile
	$(compile) -fcoarray=lib -c $(collectives_path)/collectives.f90


.PHONY : clean
clean :
	-rm -f  *.o *.mod $(executable) 
