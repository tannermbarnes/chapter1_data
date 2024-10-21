# Compute locations from <prefix>/@{LIBRARY_DIR@/cmake/lapacke-<v>/<self>.cmake
get_filename_component(_CBLAS_SELF_DIR "${CMAKE_CURRENT_LIST_FILE}" PATH)

# Load the LAPACK package with which we were built.
set(LAPACK_DIR "/usr/lib/mxe/usr/x86_64-w64-mingw32.static.posix/lib/cmake/lapack-3.12.0")
find_package(LAPACK NO_MODULE)

# Load lapacke targets from the install tree.
if(NOT TARGET cblas)
  include(${_CBLAS_SELF_DIR}/cblas-targets.cmake)
endif()

# Report lapacke header search locations.
set(CBLAS_INCLUDE_DIRS /usr/lib/mxe/usr/x86_64-w64-mingw32.static.posix/include)

# Report lapacke libraries.
set(CBLAS_LIBRARIES cblas)

unset(_CBLAS_SELF_DIR)
