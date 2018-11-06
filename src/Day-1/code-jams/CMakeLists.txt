cmake_minimum_required(VERSION 3.10)

project(fortran2018today LANGUAGES Fortran)

add_executable( test-emulated-co_sum

  02-assertion-interface.f90
  03-assertion-implementation.f90
  emulated-collectives-implementation.f90
  emulated-collectives-interface.f90
  test-emulated-co_sum.f90
)
