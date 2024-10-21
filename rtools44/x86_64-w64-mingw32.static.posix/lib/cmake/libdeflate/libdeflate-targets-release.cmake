#----------------------------------------------------------------
# Generated CMake target import file for configuration "Release".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "libdeflate::libdeflate_static" for configuration "Release"
set_property(TARGET libdeflate::libdeflate_static APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(libdeflate::libdeflate_static PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_RELEASE "C"
  IMPORTED_LOCATION_RELEASE "/usr/lib/mxe/usr/x86_64-w64-mingw32.static.posix/lib/libdeflate.a"
  )

list(APPEND _cmake_import_check_targets libdeflate::libdeflate_static )
list(APPEND _cmake_import_check_files_for_libdeflate::libdeflate_static "/usr/lib/mxe/usr/x86_64-w64-mingw32.static.posix/lib/libdeflate.a" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
