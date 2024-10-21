#----------------------------------------------------------------
# Generated CMake target import file for configuration "Release".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "libkea::kea" for configuration "Release"
set_property(TARGET libkea::kea APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(libkea::kea PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_RELEASE "CXX"
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/lib/libkea.a"
  )

list(APPEND _cmake_import_check_targets libkea::kea )
list(APPEND _cmake_import_check_files_for_libkea::kea "${_IMPORT_PREFIX}/lib/libkea.a" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
