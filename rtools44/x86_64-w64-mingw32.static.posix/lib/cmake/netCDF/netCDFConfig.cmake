# NetCDF Configuration Summary
#
# General
#

####### Expanded from @PACKAGE_INIT@ by configure_package_config_file() #######
####### Any changes to this file will be overwritten by the next CMake run ####
####### The input file was netCDFConfig.cmake.in                            ########

get_filename_component(PACKAGE_PREFIX_DIR "${CMAKE_CURRENT_LIST_DIR}/../../../" ABSOLUTE)

# Use original install prefix when loaded through a "/usr move"
# cross-prefix symbolic link such as /lib -> /usr/lib.
get_filename_component(_realCurr "${CMAKE_CURRENT_LIST_DIR}" REALPATH)
get_filename_component(_realOrig "/usr/lib/mxe/usr/x86_64-w64-mingw32.static.posix/lib/cmake/netCDF" REALPATH)
if(_realCurr STREQUAL _realOrig)
  set(PACKAGE_PREFIX_DIR "/usr/lib/mxe/usr/x86_64-w64-mingw32.static.posix")
endif()
unset(_realOrig)
unset(_realCurr)

macro(set_and_check _var _file)
  set(${_var} "${_file}")
  if(NOT EXISTS "${_file}")
    message(FATAL_ERROR "File or directory ${_file} referenced by variable ${_var} does not exist !")
  endif()
endmacro()

####################################################################################

set(NetCDFVersion "4.9.2")
set_and_check(netCDF_INSTALL_PREFIX "${PACKAGE_PREFIX_DIR}")
set_and_check(netCDF_INCLUDE_DIR "${PACKAGE_PREFIX_DIR}/include")
set_and_check(netCDF_LIB_DIR "${PACKAGE_PREFIX_DIR}/lib")

set(netCDF_LIBRARIES netCDF::netcdf)

# include target information
include("${CMAKE_CURRENT_LIST_DIR}/netCDFTargets.cmake")

# Compiling Options
#
set(netCDF_C_COMPILER "/usr/lib/mxe/usr/x86_64-pc-linux-gnu/bin/x86_64-w64-mingw32.static.posix-gcc")
set(netCDF_C_COMPILER_FLAGS "-DLIBXML_STATIC -fmacro-prefix-map='/root/mxe/tmp-netcdf-x86_64-w64-mingw32.static.posix/netcdf-c-4.9.2'=. -D_LARGEFILE64_SOURCE -D_FILE_OFFSET_BITS=64 -O3 -DNDEBUG")
set(netCDF_C_CPP_FLAGS " ")
set(netCDF_LDFLAGS " ")
set(netCDF_AM_CFLAGS "")
set(netCDF_AM_CPPFLAGS "")
set(netCDF_AM_LDFLAGS "")
set(netCDF_SHARED no)
set(netCDF_STATIC yes)

# Features:
#
set(netCDF_HAS_NC2 yes)
set(netCDF_HAS_NC4 yes)
set(netCDF_HAS_HDF4 yes)
set(netCDF_HAS_HDF5 yes)
set(netCDF_HAS_PNETCDF no)
set(netCDF_HAS_PARALLEL no)
set(netCDF_HAS_DAP yes)
set(netCDF_HAS_DAP2 yes)
set(netCDF_HAS_DAP4 yes)
set(netCDF_HAS_DISKLESS yes)
set(netCDF_HAS_MMAP no)
set(netCDF_HAS_JNA no)