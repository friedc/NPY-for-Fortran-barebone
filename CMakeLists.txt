cmake_minimum_required(VERSION 2.8)

project(NPY-for-Fortran)

enable_language(Fortran)

add_executable(example src/npy.f90 src/example.f90)

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall -Wextra -ffree-form -std=f2003")
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -stand f2003")
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "PGI")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Mfreeform -Mallocatable=03 -Mstandard")
endif()
