
echo
echo ===================================
echo GCC Os 
echo ===================================
gcc -g -Os sqrt.c -lm -o sqrt

time ./sqrt 1 0 10000
echo
time ./sqrt 1 1 10000
echo
time ./sqrt 1 2 10000
echo
time ./sqrt 1 3 10000

echo
echo ===================================
echo GCC O2 
echo ===================================
gcc -g -O2 sqrt.c -lm -o sqrt

time ./sqrt 1 0 10000
echo
time ./sqrt 1 1 10000
echo
time ./sqrt 1 2 10000
echo
time ./sqrt 1 3 10000


echo
echo ===================================
echo GCC O3 
echo ===================================
gcc -g -O3 sqrt.c -lm -o sqrt

time ./sqrt 1 0 10000
echo
time ./sqrt 1 1 10000
echo
time ./sqrt 1 2 10000
echo
time ./sqrt 1 3 10000

echo
echo ===================================
echo CLANG Os 
echo ===================================
clang -g -O2 sqrt.c -lm -o sqrt

time ./sqrt 1 0 10000
echo
time ./sqrt 1 1 10000
echo
time ./sqrt 1 2 10000
echo
time ./sqrt 1 3 10000

echo
echo ===================================
echo CLANG O2 
echo ===================================
clang -g -O2 sqrt.c -lm -o sqrt

time ./sqrt 1 0 10000
echo
time ./sqrt 1 1 10000
echo
time ./sqrt 1 2 10000
echo
time ./sqrt 1 3 10000


echo
echo ===================================
echo CLANG O3 
echo ===================================
clang -g -O3 sqrt.c -lm -o sqrt

time ./sqrt 1 0 10000
echo
time ./sqrt 1 1 10000
echo
time ./sqrt 1 2 10000
echo
time ./sqrt 1 3 10000

echo
