
# Integer SQRT Comparison

In [this tweet](https://twitter.com/field_hamster/status/1326800952586874883), Michael Field
(aka [@field_hamster](https://twitter.com/field_hamster)), showed an integer square root
algorithm that can easily be converted to an interative or pipelined FPGA implementation.

Back when I was working on my [ray tracing project](https://tomverbeure.github.io/rtl/2018/11/26/Racing-the-Beam-Ray-Tracer.html), 
I had a look at square root algorithms as well, and stumbled onto one that might even require less hardware.

See ["Implementation of Single Precision Floating Point Square Root on FPGAs"](https://yamin.cis.k.hosei.ac.jp/papers/FCCM97.pdf).

I decided to do a quick comparison between the 2 algorithms when implemented in C.

`./run.sh` will do a bunch of runs for various GCC and CLANG parameters.

In general, Michael's version is always ~25% faster. I think that this is because it uses
operations that map naturally onto a CPU ALU operations. The FPGA optimized version
relies more on bit manipulation.

I suspect that in hardware, Michael's version will require more logic.

Here are the results on my brand new AMD 5600X system:

```
===================================
GCC Os
===================================
Type: 1 - benchmark
Algo: 0 - hamster
Nr loops: 10000

real	0m3.957s
user	0m3.956s
sys	0m0.000s

Type: 1 - benchmark
Algo: 1 - fpga
Nr loops: 10000

real	0m4.369s
user	0m4.368s
sys	0m0.000s

Type: 1 - benchmark
Algo: 2 - fpga
Nr loops: 10000

real	0m4.287s
user	0m4.287s
sys	0m0.000s

===================================
GCC O2
===================================
Type: 1 - benchmark
Algo: 0 - hamster
Nr loops: 10000

real	0m3.838s
user	0m3.838s
sys	0m0.000s

Type: 1 - benchmark
Algo: 1 - fpga
Nr loops: 10000

real	0m4.474s
user	0m4.474s
sys	0m0.000s

Type: 1 - benchmark
Algo: 2 - fpga
Nr loops: 10000

real	0m4.171s
user	0m4.171s
sys	0m0.000s

===================================
GCC O3
===================================
Type: 1 - benchmark
Algo: 0 - hamster
Nr loops: 10000

real	0m2.979s
user	0m2.978s
sys	0m0.000s

Type: 1 - benchmark
Algo: 1 - fpga
Nr loops: 10000

real	0m3.701s
user	0m3.700s
sys	0m0.000s

Type: 1 - benchmark
Algo: 2 - fpga
Nr loops: 10000

real	0m3.661s
user	0m3.660s
sys	0m0.000s

===================================
CLANG Os
===================================
Type: 1 - benchmark
Algo: 0 - hamster
Nr loops: 10000

real	0m1.530s
user	0m1.530s
sys	0m0.000s

Type: 1 - benchmark
Algo: 1 - fpga
Nr loops: 10000

real	0m2.025s
user	0m2.024s
sys	0m0.000s

Type: 1 - benchmark
Algo: 2 - fpga
Nr loops: 10000

real	0m2.103s
user	0m2.099s
sys	0m0.004s

===================================
CLANG O2
===================================
Type: 1 - benchmark
Algo: 0 - hamster
Nr loops: 10000

real	0m1.540s
user	0m1.540s
sys	0m0.000s

Type: 1 - benchmark
Algo: 1 - fpga
Nr loops: 10000

real	0m2.027s
user	0m2.027s
sys	0m0.000s

Type: 1 - benchmark
Algo: 2 - fpga
Nr loops: 10000

real	0m2.104s
user	0m2.104s
sys	0m0.000s

===================================
CLANG O3
===================================
Type: 1 - benchmark
Algo: 0 - hamster
Nr loops: 10000

real	0m1.794s
user	0m1.794s
sys	0m0.000s

Type: 1 - benchmark
Algo: 1 - fpga
Nr loops: 10000

real	0m1.981s
user	0m1.981s
sys	0m0.000s

Type: 1 - benchmark
Algo: 2 - fpga
Nr loops: 10000

real	0m1.879s
user	0m1.879s
sys	0m0.000s
```
