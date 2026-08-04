cmake_minimum_required(VERSION 3.15)

project(bitrep LANGUAGES Fortran CXX)

option(BUILD_TESTINGS "Whether to build tests" ON)

set(CUDA_CC "" CACHE STRING "CUDA Compute Capability for NVHPC compiler (leave empty to skip)")
set(AMD_GPU_ARCH "" CACHE STRING "AMD GPU architecture for OpenMP target (e.g. gfx90a, gfx908)")
set(OFFLOAD_MODEL "OPENACC" CACHE STRING "GPU offload model: NONE, OPENACC, OPENMP")
set_property(CACHE OFFLOAD_MODEL PROPERTY STRINGS NONE OPENACC OPENMP)

set(HAS_CUDA OFF)
if(CMAKE_Fortran_COMPILER_ID STREQUAL "NVHPC")
  include(CheckLanguage)
  check_language(CUDA)
  if(CMAKE_CUDA_COMPILER)
    enable_language(CUDA)
    set(HAS_CUDA ON)
  else()
    message(WARNING "NVHPC detected but no CUDA compiler found - test_perf_cuda will be skipped")
  endif()
endif()

# -- Library ---------------------------------------------------------------------------------------

set(BR_SOURCES
  src/br_interface.f90
  src/br_transcendentals.cpp
)

add_library(bitrep STATIC ${BR_SOURCES})

set_source_files_properties(src/br_interface.f90 PROPERTIES Fortran_PREPROCESS ON)

set_target_properties(bitrep PROPERTIES
  LINKER_LANGUAGE             Fortran
  Fortran_MODULE_DIRECTORY    ${CMAKE_CURRENT_BINARY_DIR}/mod
  POSITION_INDEPENDENT_CODE   ON
)

target_compile_options(bitrep PRIVATE
  $<$<COMPILE_LANGUAGE:Fortran>:-O0>
  $<$<COMPILE_LANGUAGE:CXX>:-O0>
)

install(TARGETS bitrep)

# -- Tests -----------------------------------------------------------------------------------------

if(BUILD_TESTINGS)
  enable_testing()

  add_executable(test_bitrep
    tests/test_bitrep.f90
    ${BR_SOURCES}
  )
  set_source_files_properties(tests/test_bitrep.f90 PROPERTIES Fortran_PREPROCESS ON)
  set_target_properties(test_bitrep PROPERTIES LINKER_LANGUAGE Fortran)
  target_include_directories(test_bitrep PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/mod)
  # target_link_libraries(test_bitrep PRIVATE bitrep)
  add_test(NAME bitrep_test COMMAND test_bitrep)

  add_executable(test_perf
    tests/test_perf.f90
    ${BR_SOURCES}
  )
  set_source_files_properties(tests/test_perf.f90 PROPERTIES Fortran_PREPROCESS ON)
  set_target_properties(test_perf PROPERTIES LINKER_LANGUAGE Fortran)
  target_include_directories(test_perf PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/mod)
  add_test(NAME bitrep_benchmark COMMAND test_perf)

  if(HAS_CUDA)
    add_executable(test_perf_cuda
      tests/test_perf_cuda.cu
      src/br_transcendentals.cu
    )
    if(CUDA_CC)
      set_target_properties(test_perf_cuda PROPERTIES CUDA_ARCHITECTURES ${CUDA_CC})
    else()
      set_target_properties(test_perf_cuda PROPERTIES CUDA_ARCHITECTURES "all")
    endif()
    set_target_properties(test_perf_cuda PROPERTIES
      LINKER_LANGUAGE CUDA
      CUDA_SEPARABLE_COMPILATION  ON
    )
    target_compile_options(test_perf_cuda PRIVATE
      $<$<COMPILE_LANGUAGE:CUDA>:--expt-relaxed-constexpr>
    )
    add_test(NAME bitrep_benchmark_cuda COMMAND test_perf_cuda)
  else()
    message(STATUS "Skipping test_perf_cuda: requires NVHPC compiler with CUDA support")
  endif()

endif()

# -- Offload model ---------------------------------------------------------------------------------

if(OFFLOAD_MODEL STREQUAL "OPENACC")

  find_package(OpenACC REQUIRED)
  target_link_libraries(bitrep PUBLIC
    OpenACC::OpenACC_CXX OpenACC::OpenACC_Fortran
  )
  target_compile_definitions(bitrep PRIVATE BITREP_USE_OPENACC)
  target_compile_definitions(test_bitrep PRIVATE BITREP_USE_OPENACC)
  target_compile_definitions(test_perf PRIVATE BITREP_USE_OPENACC)

elseif(OFFLOAD_MODEL STREQUAL "OPENMP")
  find_package(OpenMP REQUIRED)
  target_link_libraries(bitrep PUBLIC
    OpenMP::OpenMP_CXX OpenMP::OpenMP_Fortran
  )
  target_compile_definitions(bitrep PRIVATE BITREP_USE_OPENMP)
  target_compile_definitions(test_bitrep PRIVATE BITREP_USE_OPENMP)
  target_compile_definitions(test_perf PRIVATE BITREP_USE_OPENMP)

endif()

# -- Compilers configuration -----------------------------------------------------------------------

if(CMAKE_Fortran_COMPILER_ID STREQUAL "NVHPC")

  set(NVHPC_FLAGS -Kieee -Mnofma)

  if(OFFLOAD_MODEL STREQUAL "OPENACC")
    list(APPEND NVHPC_FLAGS "-acc=gpu")
    list(APPEND NVHPC_FLAGS "-Minfo=all")
  endif()


  if(OFFLOAD_MODEL STREQUAL "OPENMP")
    list(APPEND NVHPC_FLAGS "-mp=gpu")
    list(APPEND NVHPC_FLAGS "-Minfo=all")
  endif()

  if(CUDA_CC)
    list(APPEND NVHPC_FLAGS "-gpu=cc${CUDA_CC}")
  else()
  endif()

  target_compile_options(bitrep PRIVATE
    $<$<COMPILE_LANGUAGE:Fortran>:${NVHPC_FLAGS}>
    $<$<COMPILE_LANGUAGE:CXX>:${NVHPC_FLAGS}>
  )

  target_compile_options(test_bitrep PRIVATE ${NVHPC_FLAGS})
  target_link_options(test_bitrep PRIVATE ${NVHPC_FLAGS})
  target_compile_options(test_perf PRIVATE ${NVHPC_FLAGS})
  target_link_options(test_perf PRIVATE ${NVHPC_FLAGS})

elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "Flang" AND CMAKE_CXX_COMPILER_ID STREQUAL "Clang")

  set(AMD_FLAGS
    -ffp-contract=off
    -fno-fast-math
    -fno-unsafe-math-optimizations
    -fno-reciprocal-math
  )

  if(OFFLOAD_MODEL STREQUAL "OPENMP")
    list(APPEND AMD_FLAGS -fopenmp -fopenmp-targets=amdgcn-amd-amdhsa)

    if(AMD_GPU_ARCH)
      list(APPEND AMD_FLAGS
        -Xopenmp-target=amdgcn-amd-amdhsa
        -march=${AMD_GPU_ARCH}
      )
    endif()

  endif()

  target_compile_options(bitrep PRIVATE
    $<$<COMPILE_LANGUAGE:Fortran>:${AMD_FLAGS}>
    $<$<COMPILE_LANGUAGE:CXX>:${AMD_FLAGS}>
  )

  target_compile_options(test_bitrep PRIVATE ${AMD_FLAGS})
  target_link_options(test_bitrep PRIVATE ${AMD_FLAGS})
  target_compile_options(test_perf PRIVATE ${AMD_FLAGS})
  target_link_options(test_perf PRIVATE ${AMD_FLAGS})

endif()
