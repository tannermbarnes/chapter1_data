# Generated by Boost 1.84.0

# address-model=64

if(CMAKE_SIZEOF_VOID_P EQUAL 4)
  _BOOST_SKIPPED("libboost_fiber-mt-x64.a" "64 bit, need 32")
  return()
endif()

# layout=tagged

# toolset=mgw

# link=static

if(DEFINED Boost_USE_STATIC_LIBS)
  if(NOT Boost_USE_STATIC_LIBS)
    _BOOST_SKIPPED("libboost_fiber-mt-x64.a" "static, Boost_USE_STATIC_LIBS=${Boost_USE_STATIC_LIBS}")
    return()
  endif()
else()
  if(NOT WIN32 AND NOT _BOOST_SINGLE_VARIANT)
    _BOOST_SKIPPED("libboost_fiber-mt-x64.a" "static, default is shared, set Boost_USE_STATIC_LIBS=ON to override")
    return()
  endif()
endif()

# runtime-link=shared

if(Boost_USE_STATIC_RUNTIME)
  _BOOST_SKIPPED("libboost_fiber-mt-x64.a" "shared runtime, Boost_USE_STATIC_RUNTIME=${Boost_USE_STATIC_RUNTIME}")
  return()
endif()

# runtime-debugging=off

if(Boost_USE_DEBUG_RUNTIME)
  _BOOST_SKIPPED("libboost_fiber-mt-x64.a" "release runtime, Boost_USE_DEBUG_RUNTIME=${Boost_USE_DEBUG_RUNTIME}")
  return()
endif()

# threading=multi

if(DEFINED Boost_USE_MULTITHREADED AND NOT Boost_USE_MULTITHREADED)
  _BOOST_SKIPPED("libboost_fiber-mt-x64.a" "multithreaded, Boost_USE_MULTITHREADED=${Boost_USE_MULTITHREADED}")
  return()
endif()

# variant=release

if(NOT "${Boost_USE_RELEASE_LIBS}" STREQUAL "" AND NOT Boost_USE_RELEASE_LIBS)
  _BOOST_SKIPPED("libboost_fiber-mt-x64.a" "release, Boost_USE_RELEASE_LIBS=${Boost_USE_RELEASE_LIBS}")
  return()
endif()

if(Boost_VERBOSE OR Boost_DEBUG)
  message(STATUS "  [x] libboost_fiber-mt-x64.a")
endif()

# Create imported target Boost::fiber

if(NOT TARGET Boost::fiber)
  add_library(Boost::fiber STATIC IMPORTED)

  set_target_properties(Boost::fiber PROPERTIES
    INTERFACE_INCLUDE_DIRECTORIES "${_BOOST_INCLUDEDIR}"
    INTERFACE_COMPILE_DEFINITIONS "BOOST_FIBER_NO_LIB"
  )
endif()

# Target file name: libboost_fiber-mt-x64.a

get_target_property(__boost_imploc Boost::fiber IMPORTED_LOCATION_RELEASE)
if(__boost_imploc)
  message(SEND_ERROR "Target Boost::fiber already has an imported location '${__boost_imploc}', which is being overwritten with '${_BOOST_LIBDIR}/libboost_fiber-mt-x64.a'")
endif()
unset(__boost_imploc)

set_property(TARGET Boost::fiber APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)

set_target_properties(Boost::fiber PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_RELEASE CXX
  IMPORTED_LOCATION_RELEASE "${_BOOST_LIBDIR}/libboost_fiber-mt-x64.a"
  )

set_target_properties(Boost::fiber PROPERTIES
  MAP_IMPORTED_CONFIG_MINSIZEREL Release
  MAP_IMPORTED_CONFIG_RELWITHDEBINFO Release
  )

list(APPEND _BOOST_FIBER_DEPS atomic context filesystem headers)

if(CMAKE_CONFIGURATION_TYPES)
  set_property(TARGET Boost::fiber APPEND PROPERTY INTERFACE_LINK_LIBRARIES
    "$<$<CONFIG:release>:bcrypt;synchronization>")
else()
  set_property(TARGET Boost::fiber APPEND PROPERTY INTERFACE_LINK_LIBRARIES
    bcrypt synchronization)
endif()
