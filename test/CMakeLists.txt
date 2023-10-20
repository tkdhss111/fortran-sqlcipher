set ( EXE test_sqlcipher )

set ( SRCS 
  ./test_sqlcipher.f90
  ../src/sqlcipher_interfaces.f90
  ../src/sqlcipher_interfaces_wrappers.f90
  )

set ( CMAKE_Fortran_FLAGS
  "-no-pie -ffree-line-length-none -Wno-unused-dummy-argument")

set ( CMAKE_Fortran_FLAGS_DEBUG
  "-Ddebug -O0 -fbounds-check -fbacktrace -Wall -Wno-uninitialized -Wno-maybe-uninitialized" )

set ( CMAKE_Fortran_FLAGS_RELEASE "-Drelease -O3" )

set ( CMAKE_C_FLAGS "-O -Wno-stringop-overflow")

add_executable ( ${EXE} ${SRCS} )

find_library ( LIBS
  NAMES sqlcipher
  PATHS ../sqlcipher/.libs)

target_link_libraries ( ${EXE} ${LIBS} )

add_test ( NAME ${EXE} COMMAND ${EXE} WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR} )

install ( TARGETS ${EXE} )
