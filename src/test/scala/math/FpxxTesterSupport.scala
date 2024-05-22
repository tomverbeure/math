
package math

import spinal.lib.Flow
import spinal.core._
import spinal.core.sim._
import spinal.lib.sim.FlowDriver
import spinal.lib.sim.FlowMonitor
import spinal.core.fiber.Handle

import scala.sys.process._

object Fp32 {

    def exp_bits    = 8
    def exp_mask    = (1L<<exp_bits)-1
    def mant_bits   = 23
    def mant_mask   = (1L<<mant_bits)-1
    def bias        = 127

    def asBits(f: Float) : Long  = java.lang.Float.floatToIntBits(f) & 0x00000000ffffffffL
    def asFloat(i: Int)  : Float = java.lang.Float.intBitsToFloat(i)

    def sign(f : Float) = asBits(f) >> (exp_bits + mant_bits)
    def exp(f : Float)  = (asBits(f) >> mant_bits) & exp_mask
    def mant(f : Float) = asBits(f) & mant_mask

    def isDenormal(f : Float) : Boolean = {
        exp(f) == 0 && mant(f) != 0
    }

    def isZero(f : Float) : Boolean = {
        exp(f) == 0 && mant(f) == 0
    }

    def isNaN(f : Float) : Boolean = {
        f.isNaN()
    }

    def isInfinite(f : Float) : Boolean = {
        f.isInfinite()
    }

    def isRegular(f : Float) : Boolean = {
        !isInfinite(f) && !isNaN(f) && !isDenormal(f)
    }


    def print_bits(f: Float) = {

        printf("%d ", sign(f))

        var i=exp_bits-1
        while(i>=0){
            printf("%d", (exp(f)>>i)&1)
            i-=1
        }

        printf(" ")
        i=mant_bits-1
        while(i>=0){
            printf("%d", (mant(f)>>i)&1)
            i-=1
        }
    }

    def print(f: Float) {
        Fp32.print_bits(f)
        printf("    %15e  %08x", f, Fp32.asBits(f));
    }

    def randomRegular(rand: scala.util.Random) : Float = {
        var ai : Int = 0
        var af : Float = 0.0f
        do {
            ai = rand.nextInt
            af = java.lang.Float.intBitsToFloat(ai)
        } while(!Fp32.isRegular(af))

        af
    }

}

object FpxxTesterSupport {

    def printAll(opA: Float, opB: Float, expected: Float, actual: Float) = {
        printf("op A:     ")
        Fp32.print(opA)
        printf("\n")

        printf("op B:     ")
        Fp32.print(opB)
        printf("\n")
        printf("\n")

        printf("Expected: ")
        Fp32.print(expected)
        printf("\n")

        printf("Actual  : ")
        Fp32.print(actual)
        printf("\n")
    }

    def printAll(op: Float, expected: Float, actual: Float) = {
        printf("op:       ")
        Fp32.print(op)
        printf("\n")

        printf("Expected: ")
        Fp32.print(expected)
        printf("\n")

        printf("Actual  : ")
        Fp32.print(actual)
        printf("\n")
    }

    def directedStimuli = Array[(Float, Float)](
                                (0,0), (0,1), (1,0),
                                (0,-0), (0,-1), (-1,0),
                                (1,1), (1, -1), (-1, 1), (-1, -1),
                                (100, 1), (-100, 1), (100, -1), (1, -100), (-1, 100), (-100, -1), (-1, -100),
                                (100000000, 1), (1, 100000000),
                                (100, 0.001f), (100, -0.001f),
                                (100, -99.9999f),
                                (Float.NaN, Float.NaN), (Float.NaN, 1), (Float.NaN, Float.PositiveInfinity), (Float.NaN, Float.NegativeInfinity),
                                (Float.PositiveInfinity, 1), (Float.NegativeInfinity, 1), (1, Float.PositiveInfinity), (1, Float.NegativeInfinity),
                                (Float.PositiveInfinity, Float.PositiveInfinity), (Float.PositiveInfinity, Float.NegativeInfinity), (Float.NegativeInfinity, Float.NegativeInfinity),
                                (Float.MaxValue, 1), (Float.MaxValue, Float.MaxValue), (1, Float.MaxValue),
                                (-Float.MaxValue, 1), (-Float.MaxValue, -Float.MaxValue), (-1, Float.MaxValue)
    )

    def testfloatGen(
        arguments: Seq[String]
    ) = {
        val genBin = sys.env.get("TESTFLOAT_GEN_BIN") getOrElse {
            assert(Process(Seq("make", "-C", "berkeley-softfloat-3/build/Linux-x86_64-GCC")).! == 0)
            assert(Process(Seq("make", "-C", "berkeley-testfloat-3/build/Linux-x86_64-GCC")).! == 0)

            "berkeley-testfloat-3/build/Linux-x86_64-GCC/testfloat_gen"
        }

        Process(genBin, arguments).lineStream.iterator
    }

    def parseHexCases(
        lines: Iterator[String],
        inputN: Int,
        inConf: FpxxConfig,
        outConf: FpxxConfig,
        testZeroSign: Boolean = true): Iterator[(List[FpxxHost], FpxxHost)] = {
        lines.map{l =>
            val parts = l.split(" ").map(BigInt(_, 16)).toList
            (parts.slice(0, inputN).map(FpxxHost(_, inConf, testZeroSign=testZeroSign)).toList, FpxxHost(parts(inputN), outConf, testZeroSign=testZeroSign))
        }
    }

    def testOperation[T <: Data](
        stimuli: Iterator[(List[FpxxHost], FpxxHost)],
        inputs: Flow[T],
        output: Flow[Fpxx],
        clockDomain: Handle[ClockDomain]
    ) = {
        val scoreboard = ScoreboardInOrder[FpxxHost]
        FlowDriver(inputs, clockDomain) { payload =>
            if (!stimuli.isEmpty) {
                val (inputs, expected) = stimuli.next()
                payload match {
                    case f: Fpxx =>
                        assert(inputs.length == 1)
                        f #= inputs.head
                    case v: Vec[Fpxx] =>
                        assert(inputs.length == v.length)
                        for ((p, c) <- v.zip(inputs)) p #= c
                }

                scoreboard.pushRef(expected, inputs)
                true
            } else false
        }.setFactor(1f)

        FlowMonitor(output, clockDomain) { payload =>
            scoreboard.pushDut(payload.toHost())
        }
        clockDomain.forkStimulus(2)
        clockDomain.waitActiveEdgeWhere(stimuli.isEmpty && scoreboard.ref.isEmpty)
    }

}
