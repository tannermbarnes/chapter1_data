option(MXE_USE_CCACHE "Enable ccache by default" ON)
if(MXE_USE_CCACHE)
  set(CMAKE_C_COMPILER /usr/lib/mxe/usr/x86_64-pc-linux-gnu/bin/x86_64-w64-mingw32.static.posix-gcc)
  set(CMAKE_CXX_COMPILER /usr/lib/mxe/usr/x86_64-pc-linux-gnu/bin/x86_64-w64-mingw32.static.posix-g++)
endif()
