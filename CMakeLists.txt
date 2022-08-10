cmake_minimum_required(VERSION 2.8)

project(NPY-for-Fortran)

enable_language(Fortran)

include(TestBigEndian)

TEST_BIG_ENDIAN(IS_BIG_ENDIAN)

if(IS_BIG_ENDIAN)
  message(FATAL_ERROR "This library does not work on big endian architecures!")
endif()

add_library(npy_o OBJECT src/npy.f90)

add_executable(example $<TARGET_OBJECTS:npy_o> src/example.f90)

add_custom_command(
  TARGET example POST_BUILD
  COMMAND cp -f $<TARGET_OBJECTS:npy_o> npy.o
  BYPRODUCTS npy.o
  VERBATIM)

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall -Wextra -ffree-form -std=f2003")
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -stand f2003")
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "PGI")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Mfreeform -Mallocatable=03 -Mstandard")
endif()
