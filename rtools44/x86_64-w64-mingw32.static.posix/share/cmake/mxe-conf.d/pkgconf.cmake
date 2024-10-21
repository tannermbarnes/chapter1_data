if(PKG_CONFIG_FOUND)
  message(FATAL_ERROR "
  ** find_package(PkgConfig) or (deprecated) include(FindPkgConfig)
  ** must be invoked after project() command when using CMAKE_TOOLCHAIN_FILE
  ")
endif()
set(PKG_CONFIG_EXECUTABLE /usr/lib/mxe/usr/bin/x86_64-w64-mingw32.static.posix-pkg-config CACHE PATH "pkg-config executable")
