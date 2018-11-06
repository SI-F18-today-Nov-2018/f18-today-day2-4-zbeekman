cmake_minimum_required(VERSION 3.12)

project(day_2_code_jams LANGUAGES Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)
include_directories(${CMAKE_BINARY_DIR}/mod)

add_library( utilities
  Day-1/code-jams/02-assertion-interface.f90
  Day-1/code-jams/03-assertion-implementation.f90
)

add_executable( test-emulated-co_sum
  emulated-collectives-implementation.f90
  emulated-collectives-interface.f90
  test-emulated-co_sum.f90
)

target_link_libraries( test-emulated-co_sum
  utilities
)

add_executable( collective-load-check
  05-collective-load-check.f90
)
target_link_libraries( collective-load-check
  utilities
)
configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/grid-parameters.nml"
  "${CMAKE_CURRENT_BINARY_DIR}/grid-parameters.nml"
  COPYONLY
)

