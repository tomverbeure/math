
# Introduction

This project contains a library of math-related hardware units.

Right now, it contains only "Fpxx" units: floating point with a user-programmable number of exponent and 
mantissa.

#### Table of Contents

- [Fpxx General](#fpxx)
- [FpxxAdd](#fpxxadd)
- [FpxxMul](#fpxxmul)
- [FpxxDiv](#fpxxdiv)
- [FpxxSqrt](#fpxxsqrt)
- [FpxxRSqrt](#fpxxrsqrt)
- [Math Related Literature](#math-related-literature)

# Fpxx

The Fpxx library is one that supports floating point operations for which the exponent and mantissa can be 
specified at compile time.

The primary use of this library is for FPGA projects that need floating point, but don't necessarily need 
all the features and precision of 32-bit standard floating point operations. By reducing the size of the
mantissa and exponent, the hardware of some floating point operations can be made to map directly onto the hardware
multipliers of the DSPs that are often present in today's FPGAs, and the maximum clock speed can be increased
significantly.

For example, many FPGAs support 18x18 bit multiplications. By restricting the size of the mantissa, a single
hardware multiplier may be sufficient to implement the core operation of the a floating point multiplier.

Goals:

* SpinalHDL

    The code is written in SpinalHDL instead of Verilog or VHDL. This makes it much easier to write generic code
    with programmable widths and pipeline stages. It also cuts back on boiler plate code.

    That said, it's almost trivial to generate the Verilog or VHDL for use in your own project. And if that's too
    much effort, a number of configuration are pre-generated and stored as Verilog and VHDL in the repository, so
    they can be copied straight into your own project.

* Floating port support for all basic operations

    At the minimum, add, multiply and divide should work with *acceptable* accuracy, whatever that means.

    For additional operations (e.g. sqrt and 1/sqrt), accuracy may very well be completely unacceptable: 
    depending on my use cases, a small lookup table could be sufficient and the library won't have a better
    solution.

* User-programmable mantissa and exponent size

    There are some limitations. For example, FpxxDiv currently requires an odd numbered mantissa.

* User-programmable size of various lookup tables or internal results

    The user may want to specify a particular mantissa, but still restrict the precion for select operations
    when it's clear that the full precision won't be needed.

    For example, one may want to use a 20-bit size mantissa in general, but restrict multiplications to 
    17 or 18 bits to map to a single FPGA DSP multiplier.

    Similarly, the divide operation uses lookup table. For certain input ranges, the size and precision of this lookup
    table may not be as larges recommended for maximum precision. 

    Where possible, the library provides knobs to play with this. 

* Support for NaN, Infinity, and sign checking

    It's important that NaN and Infinity values get propagated through the pipeline, to avoid cases where these kind of
    values alias into a real value. NaN number should be generated for operations such as asking for the square root of
    a negative. Overflows or division by 0 will result in Infinity.

* One result per cycle

    The library is initially designed for a use cases where one result is needed per clock cycle. 

* User-programmable pipeline depth

    For each instance, the user can control the amount of intermediate pipeline stages. This makes it possible to trade
    off between clock speed, pipeline latency and clock speed.

* C++ model

    There is C++ template class with an implementation of the Fpxx modules.

    This can be very useful to first create a C++ proof of concept of your design before implementing it in hardware.

    The goal is for the C++ model and the hardware model to be bit exact (though this might not always be the case.)

* Testbench

    A testbench with directed and random vectors is provided to verify the results between a model that has a 
    23-bit mantissa and 8-bit exponent and the standard IEEE fp32 operations of your PC.

    The testbench ignores differences that are due to the limitations of the library (e.g. denormals, rounding 
    differences etc.)

Non-goals:

* Support for denormals

    Denormals requires quite a bit of additional logic for often little benefit. Support for them may be added later,
    there it's not there at this time.

    When a denormal is encountered on an input, it is immediately clamped to zero. Denormal results are replaced by
    a zero as well.

* (Correct) rounding

    Rounding is a surprisingly expensive operation and hard to get really right. At this moment, it is not supported at all.
    This has definitely an impact on precision.

* Correct handling of negative and positive zeros

    For some operations, negative and positive zeros are dealt with correctly, but not all of them.

## FpxxAdd

## FpxxMul

## FpxxDiv

## FpxxSqrt

## FpxxRSqrt


# Math Related Literature

## Reduced Precision Floating Point

* [Simplified Floating Point for DSP](https://people.ece.cornell.edu/land/courses/ece5760/FloatingPoint/index.html)

    Cornell student project with C code and Verilog.

* [Float Point Core generator](http://perso.citi-lab.fr/fdedinec/recherche/2013-HiPEAC-Tutorial-FloPoCo/)

    Create custom VHDL floating point cores of variable size.

## Articles on two-complement floating point

* [TMS320C3x User Guide (1994)](https://www.ele.uva.es/~jesman/BigSeti/ftp/DSPs/Texas_Instrument_TMS320Cxx/TMS320C3x-J.pdf)

    Old user guide.  Two-complements floating point section starts at page 4-4.

* [TMS320C3x User Guide (2004)](http://www.ti.com/lit/ug/spru031f/spru031f.pdf)

    * Current official version. Has typos. E.g. bottom of 5-35 is incorrect.

* [StackExchange question](https://electronics.stackexchange.com/questions/223832/tms320-floating-point-texas-instruments-dsp-from-98)

    Links to conversion code.

## Division

* [Variable Precision Floating Point Division and Square Root](http://www.dtic.mil/dtic/tr/fulltext/u2/a433087.pdf)

    Very interesting presentation on how to create division and square root on FPGA.

    The thesis about this presentation can be found [here](http://www.ece.neu.edu/groups/rcl/theses/xjwang_phd2007.pdf).

    [Another thesis](https://scholarworks.rit.edu/cgi/viewcontent.cgi?article=10862&context=theses) implementing this kind of divider, with (bad) source code.

* [A Pipelined Divider with a Small Lookup Table](http://www.wseas.us/e-library/conferences/2007hangzhou/papers/560-407.pdf)

    Paper that describes a similar divider as the one in the presentation above, but with smaller lookup table and more multipliers.

* [Fast Division Algorithm with a Small Lookup Table](http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=21713E8242DF34DCC6DE68623F47AC1F?doi=10.1.1.43.3795&rep=rep1&type=pdf)

    Paper that is referenced by the two papers above as main inspiration for the LUT + 2 multipliers division operation.

    Includes detailed mathematical derivation and error analysis.


## Square Root and Reciprocal Square Root

* [Matlab - Implement Fixed-Point Square Root Using Lookup Table](https://www.mathworks.com/help/fixedpoint/examples/implement-fixed-point-square-root-using-lookup-table.html)

    Matlab code for fixed point square root lookup table.

* [Variable Precision Floating Point Division and Square Root](http://www.dtic.mil/dtic/tr/fulltext/u2/a433087.pdf)

    Uses combination of table lookup and a bunch of multipliers for square root. See same paper under the 'Division' section for related information.

* [Implementation of Single Precision Floating Point Square Root on FPGAs](https://pdfs.semanticscholar.org/1c3c/569f613024e8164450daf05a01163c99f72c.pdf)

    Shows simple interative implementation and pipelined version, both for integer-only and floating point.

    FP32 version requires 15 pipeline stages instead of 24, because some stages are so small that they can be collapsed.

    Does *not* use a lookup table or multiplier, just a bunch of adders.

* [An Optimized Square Root Algorithm for Implementation in FPGA Hardware](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.174.6832&rep=rep1&type=pdf)

    Seems to be equivalent to the previous one.

* [Reciprocation, Square root, Inverse Square Root, and some Elementary Functions using Small Multipliers](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.30.7789&rep=rep1&type=pdf)

    Paper that is referenced by the papers above as main inspiration for the LUT + multipliers approach.

    Had detailed mathematical derivation about how things work.

* [Methods of Computing Square Roots](https://en.wikipedia.org/wiki/Methods_of_computing_square_roots)

    Wikipedia.

* [Best Square Root Methods](https://www.codeproject.com/Articles/69941/Best-Square-Root-Method-Algorithm-Function-Precisi)

    Not very useful.

* [Simple Seed Architectures for Reciprocal and Square Root Reciprocal](http://perso.ens-lyon.fr/jean-michel.muller/EMTAsilomar05.pdf)

    Not very useful.

* [Fixed-Point Implementations of the Reciprocal, Square Root and Reciprocal Square Root Functions](https://hal.archives-ouvertes.fr/hal-01229538/file/fxpfunc.pdf)

* [Ask Hackaday: Computing Square Roots on FPGA?](https://hackaday.com/2016/12/26/ask-hackaday-computing-square-roots-on-fpga/)

* [Chebyshev Approximation and How It Can Help You Save Money, Win Friends, and Influence People - Jason Sachs](https://www.embeddedrelated.com/showarticle/152.php)

* [Fast interactive sqrt](https://pdfs.semanticscholar.org/5060/4e9aff0e37089c4ab9a376c3f35761ffe28b.pdfv)


## Leading Zero Counter (LZC) and Leading Zero Anticipor (LZA)

* [Modular Design of Fast Leading Zeros Counting Circuit](https://www.degruyter.com/downloadpdf/j/jee.2015.66.issue-6/jee-2015-0054/jee-2015-0054.pdf)

   Very fast and low area regular leading zero counting implementation.
   
* [Stack Exchange Hierarchical Solution](https://electronics.stackexchange.com/questions/196914/verilog-synthesize-high-speed-leading-zero-count)

   Neat implementation, but apparently not nearly as area and speed efficient as the implementation of the previous bullet point.
   (See also [this video](https://www.youtube.com/watch?v=lZ1DqG0Pn_I)

* [Leading-Zero Anticipatory Logic for High-Speed Floating Point Addition](http://soc.knu.ac.kr/video_lectures/11_7.pdf) (1995)

* [Leading Zero Anticipation and Detection - A Comparison of Methods](https://www.csee.umbc.edu/~phatak/645/supl/lza/lza-survey-arith01.pdf) (2001) 

* [Analysis and Implementation of a Novel Leading Zero Anticipation Algorithm for Floating Point Arithmetic Units](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.667.533&rep=rep1&type=pdf) (2001)

* [Hybrid LZA: A Near Optimal Implementation of the Leading Zero Anticipator](https://lap.epfl.ch/files/content/sites/lap/files/shared/publications/VermaJan09_HybridLzaANearOptimalImplementationOfTheLeadingZeroAnticipator_ASPDAC09.pdf) (2009)

# Sin/Cos Calculation

* [Computing sin & cos in hardware with synthesisable Verilog](http://kierdavis.com/cordic.html)
