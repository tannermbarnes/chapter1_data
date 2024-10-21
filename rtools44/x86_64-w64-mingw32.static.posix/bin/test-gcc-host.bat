set PATH=..\bin;%PATH%
gcc test-gcc-host.c -o test-gcc-host.exe -fopenmp -v
test-gcc-host.exe
strip test-gcc-host.exe test-pthreads-libgomp.exe
fc /b test-gcc-host.exe test-pthreads-libgomp.exe
cmd
