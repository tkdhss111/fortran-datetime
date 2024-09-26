set ( EXE test_datetime )
set ( SRCS ./test.f90
           ../src/dt_mo.f90
    )

set ( CMAKE_Fortran_FLAGS
  "-ffree-line-length-none -fcoarray=single")
set ( CMAKE_Fortran_FLAGS_DEBUG
  "-Ddebug -O0 -fbounds-check -fbacktrace -Wall -Wno-uninitialized -Wno-maybe-uninitialized" )
set ( CMAKE_Fortran_FLAGS_RELEASE "-Drelease -O3" )
set ( CMAKE_C_FLAGS "-O -Wno-stringop-overflow")

add_executable ( ${EXE} ${SRCS} )
target_link_libraries ( ${EXE} ${LIBS} )
add_test ( NAME ${EXE} COMMAND ${EXE} ${ARGS} WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR} )
install ( TARGETS ${EXE} DESTINATION bin )
