cmake_minimum_required(VERSION 3.12)

project(day_2 LANGUAGES Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)
include_directories(${CMAKE_BINARY_DIR}/mod)
add_library( utilities
  Day-1/code-jams/02-assertion-interface.f90
  Day-1/code-jams/03-assertion-implementation.f90
)

add_executable( test-collective-load-check
  05-collective-load-check.f90
)

target_link_libraries( test-collective-load-check
  utilities
)

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/grid.nml"
  "${CMAKE_BINARY_DIR}/grid.nml"
  COPYONLY
)
