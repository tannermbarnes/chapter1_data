GENCCODE_ASSEMBLY_TYPE=-a gcc-mingw64
SO=dll
SOBJ=dll
A=a
LIBPREFIX=s
LIB_EXT_ORDER=74.dll
COMPILE=x86_64-w64-mingw32.static.posix-gcc   -DHAVE_DLOPEN=0 -DU_HAVE_MMAP=0 -DU_HAVE_STRTOD_L=0  -DU_STATIC_IMPLEMENTATION -DWINVER=0x0601 -D_WIN32_WINNT=0x0601 -DU_ATTRIBUTE_DEPRECATED= -O2 -std=c11 -Wall -pedantic -Wshadow -Wpointer-arith -Wmissing-prototypes -Wwrite-strings -mthreads  -c
LIBFLAGS=-I/usr/lib/mxe/usr/x86_64-w64-mingw32.static.posix/include -DPIC 
GENLIB=x86_64-w64-mingw32.static.posix-gcc -O2 -std=c11 -Wall -pedantic -Wshadow -Wpointer-arith -Wmissing-prototypes -Wwrite-strings -mthreads   -shared  -Wl,--enable-auto-import -Wl,--out-implib=./liball.dll.a
LDICUDTFLAGS=
LD_SONAME=
RPATH_FLAGS=
BIR_LDFLAGS=
AR=x86_64-w64-mingw32.static.posix-ar
ARFLAGS=r
RANLIB=x86_64-w64-mingw32.static.posix-ranlib
INSTALL_CMD=/usr/bin/install -c