cmake_minimum_required(VERSION 3.12)

project(day_3_hands_on LANGUAGES Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)
include_directories(${CMAKE_BINARY_DIR}/mod)

add_library( utilities
  Day-1/code-jams/02-assertion-interface.f90
  Day-1/code-jams/03-assertion-implementation.f90
)

add_executable( test-functional-grid
  test-functional-grid.f90
  functional-grid-interface.f90
  solution/structured-grid-implementation.f90
  solution/structured-grid-interface.f90
)

target_link_libraries( test-functional-grid
  utilities
)

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/grid-parameters.nml"
  "${CMAKE_CURRENT_BINARY_DIR}/grid-parameters.nml"
  COPYONLY
)
