# Copyright 2022 The RE2 Authors.  All Rights Reserved.
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file.


####### Expanded from @PACKAGE_INIT@ by configure_package_config_file() #######
####### Any changes to this file will be overwritten by the next CMake run ####
####### The input file was re2Config.cmake.in                            ########

get_filename_component(PACKAGE_PREFIX_DIR "${CMAKE_CURRENT_LIST_DIR}/../../../" ABSOLUTE)

# Use original install prefix when loaded through a "/usr move"
# cross-prefix symbolic link such as /lib -> /usr/lib.
get_filename_component(_realCurr "${CMAKE_CURRENT_LIST_DIR}" REALPATH)
get_filename_component(_realOrig "/usr/lib/mxe/usr/x86_64-w64-mingw32.static.posix/lib/cmake/re2" REALPATH)
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

macro(check_required_components _NAME)
  foreach(comp ${${_NAME}_FIND_COMPONENTS})
    if(NOT ${_NAME}_${comp}_FOUND)
      if(${_NAME}_FIND_REQUIRED_${comp})
        set(${_NAME}_FOUND FALSE)
      endif()
    endif()
  endforeach()
endmacro()

####################################################################################

include(CMakeFindDependencyMacro)

set_and_check(re2_INCLUDE_DIR ${PACKAGE_PREFIX_DIR}/include)

if(UNIX)
  set(THREADS_PREFER_PTHREAD_FLAG ON)
  find_dependency(Threads REQUIRED)
endif()

find_dependency(absl REQUIRED)

if(OFF)
  find_dependency(ICU REQUIRED COMPONENTS uc)
endif()

check_required_components(re2)

if(TARGET re2::re2)
  return()
endif()

include(${CMAKE_CURRENT_LIST_DIR}/re2Targets.cmake)
