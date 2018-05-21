./executables/fibonacci.exe --output ./output/fibonacci.html

./executables/fibonacci.exe --regress allocated:iters +RTS -T > ./output/fibonacci_bytesalloc_prof.log

./executables/fibonacci.exe --regress numGcs:iters +RTS -T > ./output/fibonacci_ghcalloc_prof.log

./executables/fibonacci.exe --regress cycles:iters +RTS -T > ./output/fibonacci_cpucycles_prof.log
