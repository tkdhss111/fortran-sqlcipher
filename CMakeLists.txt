cmake_minimum_required ( VERSION 3.0 )

set ( CMAKE_Fortran_COMPILER gfortran )
set ( CMAKE_C_COMPILER gcc )

enable_language ( Fortran C )
enable_testing ()

project ( sqlcipher LANGUAGES Fortran VERSION 1.0.0 )

add_subdirectory ( test )
