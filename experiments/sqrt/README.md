
# Integer SQRT Comparison

In [this tweet](https://twitter.com/field_hamster/status/1326800952586874883), Michael Field
(aka [@field_hamster](https://twitter.com/field_hamster)), showed an integer square root
algorithm that can easily be converted to an interative or pipelined FPGA implementation.

Back when I was working on my [ray tracing project](https://tomverbeure.github.io/rtl/2018/11/26/Racing-the-Beam-Ray-Tracer.html), 
I had a look at square root algorithms as well, and stumbled onto one that might even require less hardware.

See ["Implementation of Single Precision Floating Point Square Root on FPGAs"](https://yamin.cis.k.hosei.ac.jp/papers/FCCM97.pdf) by
Yamin et alt.

I decided to do a quick comparison between the 2 algorithms when implemented in C.

`./run.sh` will do a bunch of runs for various GCC and CLANG parameters.

In general, Michael's version is always ~25% faster. I think that this is because it uses
operations that map naturally onto a CPU ALU operations. The Yamin's version
relies more on bit manipulation.

The [Godbolt Compiler Explorer versions of the code](https://godbolt.org/z/aejjnh). 

I suspect that in hardware, Michael's version will require more logic.

After this, I started looking for more algorithms. There are tons of them.

I tried out this one by Sutikno: 

[An Efficient Implementation of the Non Restoring Square Root Algorithm in Gate Level](http://www.ijcte.org/papers/281-G850.pdf).

In one case, it does better than the one of Michael.  It's similar to Yamin's version in that it has an adder that 
increases for each step.

Here are the results on my brand new AMD 5600X system:

```
===================================
GCC Os
===================================
Type: 1 - benchmark
Algo: 0 - hamster
Nr loops: 10000

real	0m3.847s
user	0m3.847s
sys	0m0.000s

Type: 1 - benchmark
Algo: 1 - yamin_rev1
Nr loops: 10000

real	0m4.372s
user	0m4.372s
sys	0m0.000s

Type: 1 - benchmark
Algo: 2 - yamin_rev2
Nr loops: 10000

real	0m4.220s
user	0m4.220s
sys	0m0.000s

Type: 1 - benchmark
Algo: 3 - sutikno
Nr loops: 10000

real	0m1.634s
user	0m1.634s
sys	0m0.000s

===================================
GCC O2
===================================
Type: 1 - benchmark
Algo: 0 - hamster
Nr loops: 10000

real	0m3.882s
user	0m3.881s
sys	0m0.000s

Type: 1 - benchmark
Algo: 1 - yamin_rev1
Nr loops: 10000

real	0m4.448s
user	0m4.447s
sys	0m0.000s

Type: 1 - benchmark
Algo: 2 - yamin_rev2
Nr loops: 10000

real	0m4.428s
user	0m4.427s
sys	0m0.000s

Type: 1 - benchmark
Algo: 3 - sutikno
Nr loops: 10000

real	0m4.185s
user	0m4.180s
sys	0m0.004s

===================================
GCC O3
===================================
Type: 1 - benchmark
Algo: 0 - hamster
Nr loops: 10000

real	0m2.999s
user	0m2.999s
sys	0m0.000s

Type: 1 - benchmark
Algo: 1 - yamin_rev1
Nr loops: 10000

real	0m3.750s
user	0m3.749s
sys	0m0.000s

Type: 1 - benchmark
Algo: 2 - yamin_rev2
Nr loops: 10000

real	0m3.667s
user	0m3.666s
sys	0m0.000s

Type: 1 - benchmark
Algo: 3 - sutikno
Nr loops: 10000

real	0m3.634s
user	0m3.630s
sys	0m0.004s

===================================
CLANG Os
===================================
Type: 1 - benchmark
Algo: 0 - hamster
Nr loops: 10000

real	0m1.498s
user	0m1.498s
sys	0m0.000s

Type: 1 - benchmark
Algo: 1 - yamin_rev1
Nr loops: 10000

real	0m2.005s
user	0m2.005s
sys	0m0.000s

Type: 1 - benchmark
Algo: 2 - yamin_rev2
Nr loops: 10000

real	0m2.103s
user	0m2.103s
sys	0m0.000s

Type: 1 - benchmark
Algo: 3 - sutikno
Nr loops: 10000

real	0m1.674s
user	0m1.674s
sys	0m0.000s

===================================
CLANG O2
===================================
Type: 1 - benchmark
Algo: 0 - hamster
Nr loops: 10000

real	0m1.496s
user	0m1.495s
sys	0m0.000s

Type: 1 - benchmark
Algo: 1 - yamin_rev1
Nr loops: 10000

real	0m2.005s
user	0m2.005s
sys	0m0.000s

Type: 1 - benchmark
Algo: 2 - yamin_rev2
Nr loops: 10000

real	0m2.104s
user	0m2.104s
sys	0m0.000s

Type: 1 - benchmark
Algo: 3 - sutikno
Nr loops: 10000

real	0m1.675s
user	0m1.675s
sys	0m0.000s

===================================
CLANG O3
===================================
Type: 1 - benchmark
Algo: 0 - hamster
Nr loops: 10000

real	0m1.795s
user	0m1.795s
sys	0m0.000s

Type: 1 - benchmark
Algo: 1 - yamin_rev1
Nr loops: 10000

real	0m1.981s
user	0m1.981s
sys	0m0.000s

Type: 1 - benchmark
Algo: 2 - yamin_rev2
Nr loops: 10000

real	0m1.877s
user	0m1.876s
sys	0m0.000s

Type: 1 - benchmark
Algo: 3 - sutikno
Nr loops: 10000

real	0m1.384s
user	0m1.383s
sys	0m0.000s
```
