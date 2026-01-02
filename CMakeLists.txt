cmake_minimum_required(VERSION 3.15)

project(bitrep LANGUAGES Fortran CXX)

option(BUILD_TESTINGS "Whether to build tests" ON)

# -- Library ---------------------------------------------------------------------------------------

add_library(bitrep STATIC
  src/br_interface.f90
  src/br_transcendentals.cpp
)

set_target_properties(bitrep PROPERTIES
  LINKER_LANGUAGE             Fortran
  Fortran_MODULE_DIRECTORY    ${CMAKE_CURRENT_BINARY_DIR}/mod
  POSITION_INDEPENDENT_CODE   ON
)

target_compile_options(bitrep PRIVATE
  $<$<COMPILE_LANGUAGE:Fortran>:-O0>
  $<$<COMPILE_LANGUAGE:CXX>:-O0>
)

find_package(OpenACC REQUIRED)
target_link_libraries(bitrep PUBLIC
  OpenACC::OpenACC_CXX OpenACC::OpenACC_Fortran
)

install(TARGETS bitrep)

# -- Tests -----------------------------------------------------------------------------------------

if(BUILD_TESTINGS)
  enable_testing()

  add_executable(test_bitrep tests/test_bitrep.f90)
  target_include_directories(test_bitrep PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/mod)
  target_link_libraries(test_bitrep PRIVATE bitrep)
  set_target_properties(test_bitrep PROPERTIES LINKER_LANGUAGE Fortran)

  add_test(NAME bitrep_test COMMAND test_bitrep)
endif()
