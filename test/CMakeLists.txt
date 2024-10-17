set ( EXE unit_test )
set ( SRCS ./unit_test.f90
           ../src/dt_mo.f90
    )

set ( CMAKE_Fortran_FLAGS
  "-ffree-line-length-none -fdec-structure -fcoarray=single")
set ( CMAKE_Fortran_FLAGS_DEBUG
  "-Ddebug -O0 -fbounds-check -fbacktrace -Wall -Wno-uninitialized -Wno-maybe-uninitialized" )

add_executable ( ${EXE} ${SRCS} )
add_test ( NAME ${EXE} COMMAND ${EXE} WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR} )
