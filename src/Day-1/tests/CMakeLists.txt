set( classes  cartesian-grid )
set( num_fortran_images  4 )

foreach( class ${classes} )
  add_executable( test-${class} test-${class}.f90 )
  add_test(NAME ${class}-test COMMAND cafrun -n ${num_fortran_images} ./test-${class})
  set_tests_properties(PROPERTIES PASS_REGULAR_EXPRESSION "Test passed.")
endforeach()
