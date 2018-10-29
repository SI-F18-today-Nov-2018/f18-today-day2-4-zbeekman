cmake_minimum_required(VERSION 3.10)

project(narrative LANGUAGES Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

enable_testing()

set( directory_list
  grid
  tests
)
foreach(directory ${directory_list})
   add_subdirectory("${directory}")
endforeach()
