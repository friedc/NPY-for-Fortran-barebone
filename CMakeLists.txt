cmake_minimum_required(VERSION 2.8)
project(STB)

file(GLOB_RECURSE sources src/*.f90 )

add_executable(npy.x ${sources})

enable_language(Fortran)
set(CMAKE_Fortran_COMPILER ifort)
set(CMAKE_Fortran_COMPILER_ID "Intel")

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(dialect "-Wall -Wextra -ffree-form -std=f2003")
    #set(bounds "-fbounds-check")
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    set(dialect "-stand f2003")
    #set(bounds "-check bounds")
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "PGI")
    set(dialect "-Mfreeform -Mallocatable=03 -Mstandard")
    #set(bounds "-C")
endif()

set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${bounds}")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${dialect}")

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")
MESSAGE( STATUS "cmake_module_path:    " ${CMAKE_MODULE_PATH})
