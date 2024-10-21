#----------------------------------------------------------------
# Generated CMake target import file for configuration "Release".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "sbml" for configuration "Release"
set_property(TARGET sbml APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(sbml PROPERTIES
  IMPORTED_IMPLIB_RELEASE "${_IMPORT_PREFIX}/lib/libsbml.dll.a"
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/libsbml.dll"
  )

list(APPEND _cmake_import_check_targets sbml )
list(APPEND _cmake_import_check_files_for_sbml "${_IMPORT_PREFIX}/lib/libsbml.dll.a" "${_IMPORT_PREFIX}/bin/libsbml.dll" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
