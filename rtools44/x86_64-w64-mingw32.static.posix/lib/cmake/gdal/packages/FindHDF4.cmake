#
# To be used by projects that make use of CMakeified hdf-4.2
#

#[=======================================================================[.rst:
FindHDF4
---------

Find the HDF4 includes and get all installed hdf4 library settings

The following vars are set if hdf4 is found.

.. variable:: HDF4_FOUND

  True if found, otherwise all other vars are undefined

IMPORTED targets
^^^^^^^^^^^^^^^^
This module defines the following :prop_tgt:`IMPORTED` target:
``HDF4::HDF4``
#
#]=======================================================================]
#

# The original code is modified for use with MXE where hdf4 is configured
# with autotools and .pc files are available.

find_package(PkgConfig QUIET)
if(PKG_CONFIG_FOUND)
  pkg_check_modules(PC_Hdf4df QUIET IMPORTED_TARGET df)
  if(PC_Hdf4df_VERSION)
    set(HDF4_VERSION_STRING ${PC_Hdf4df_VERSION})
  endif()
endif()
if(PC_Hdf4df_FOUND)
  if(NOT TARGET PkgConfig::Hdf4df)
    add_library(PkgConfig::Hdf4df INTERFACE IMPORTED)
    set_target_properties(PkgConfig::Hdf4df PROPERTIES INTERFACE_LINK_LIBRARIES PkgConfig::PC_Hdf4df)
  endif()
  list(APPEND HDF4_TARGETS PkgConfig::Hdf4df)
  pkg_check_modules(PC_Hdf4mfhdf QUIET IMPORTED_TARGET mfhdf)
  if(PC_Hdf4mfhdf_FOUND)
    if(NOT TARGET PkgConfig::Hdf4mfhdf)
      add_library(PkgConfig::Hdf4mfhdf INTERFACE IMPORTED)
      set_target_properties(PkgConfig::Hdf4mfhdf PROPERTIES INTERFACE_LINK_LIBRARIES PkgConfig::PC_Hdf4mfhdf)
    endif()
    list(APPEND HDF4_TARGETS PkgConfig::Hdf4mfhdf)
  endif()
endif()

find_path(
  HDF4_INCLUDE_DIR NAMES "hdf.h"
  HINTS ${PC_Hdf4df_INCLUDE_DIRS})

find_library(HDF4DF_LIBRARY NAMES df HINTS ${PC_Hdf4df_LIBRARY_DIRS})
find_library(HDF4MFHDF_LIBRARY NAMES mfhdf HINTS ${PC_Hdf4mfhdf_LIBRARY_DIRS})

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(HDF4
                                  FOUND_VAR HDF4_FOUND
                                  REQUIRED_VARS HDF4DF_LIBRARY HDF4_INCLUDE_DIR
                                  VERSION_VAR HDF4_VERSION_STRING
                                  HANDLE_COMPONENTS
                                  )

if(HDF4_FOUND)
  set(HDF4_INCLUDE_DIRS "${HDF4_INCLUDE_DIR}")
  if(NOT TARGET HDF4::HDF4)
    add_library(HDF4::HDF4 INTERFACE IMPORTED)
    set_target_properties(HDF4::HDF4 PROPERTIES
                          INTERFACE_INCLUDE_DIRECTORIES "${HDF4_INCLUDE_DIR}"
                          INTERFACE_LINK_LIBRARIES "${HDF4_TARGETS}")
  endif()
endif()
