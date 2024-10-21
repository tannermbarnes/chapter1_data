# Compute locations from <prefix>/@{LIBRARY_DIR@/cmake/lapacke-<v>/<self>.cmake
get_filename_component(_LAPACKE_SELF_DIR "${CMAKE_CURRENT_LIST_FILE}" PATH)

# Load the LAPACK package with which we were built.
set(LAPACK_DIR "/usr/lib/mxe/usr/x86_64-w64-mingw32.static.posix/lib/cmake/lapack-3.12.0")
find_package(LAPACK NO_MODULE)

# Load lapacke targets from the install tree.
if(NOT TARGET lapacke)
  include(${_LAPACKE_SELF_DIR}/lapacke-targets.cmake)
endif()

# Hint for project building against lapack
set(LAPACKE_Fortran_COMPILER_ID ${LAPACK_Fortran_COMPILER_ID})

# Report lapacke header search locations.
set(LAPACKE_INCLUDE_DIRS "")

# Report lapacke libraries.
set(LAPACKE_LIBRARIES lapacke ${LAPACK_LIBRARIES})

unset(_LAPACKE_SELF_DIR)
