
package math

import spinal.sim._
import spinal.core._
import spinal.core.sim._

object FpxxDemoTests {

    def print_bits(f: Float) = {
        var fl : Long = java.lang.Float.floatToIntBits(f) & 0x00000000ffffffffL

        printf("%d ", (fl>>30)&1)
        var i=30
        while(i>=23){
            printf("%d", (fl>>i)&1)
            i-=1
        }
        printf(" ")
        while(i>=0){
            printf("%d", (fl>>i)&1)
            i-=1
        }
    }

    def countLeadingZeros(lz_in: Long) : Int = {
        var i=22
        var nrZeros = 0
        while(i>=0 && (lz_in&(1<<i))==0){
            nrZeros += 1
            i -= 1
        }
        nrZeros
    }

    def isDenormal(f : Float) : Boolean = {
        val fi : Int = java.lang.Float.floatToIntBits(f)

        (((fi>>23) & 0xff) == 0) && ((fi & 0x007fffff) != 0)
    }

    def isRegular(f : Float) : Boolean = {
        !f.isInfinite() && !f.isNaN() && !isDenormal(f)
    }

    def randomNormalFloat(rand: scala.util.Random) : Float = {
        var ai : Int = 0
        var af : Float = 0.0f
        do {
            ai = rand.nextInt
            af = java.lang.Float.intBitsToFloat(ai)
        } while(!isRegular(af))

        af
    }

    def resultMatches(opA: Float, opB: Float, expected: Float, actual: Float, verbose: Boolean = false) : Boolean = {

        val actualMant   : Long = java.lang.Float.floatToIntBits(actual)   & 0x00000000007fffffL
        val expectedMant : Long = java.lang.Float.floatToIntBits(expected) & 0x00000000007fffffL

        if ((actualMant-expectedMant).abs > 15 && isRegular(expected)){
            printf("ERROR!\n")
            printf("op A:     ")
            print_bits(opA)
            printf("    %15e  %08x\n", opA, java.lang.Float.floatToIntBits(opA));

            printf("op B:     ")
            print_bits(opB)
            printf("    %15e  %08x\n", opB, java.lang.Float.floatToIntBits(opB));
            printf("\n")

            printf("Expected: ")
            print_bits(expected)
            printf("    %15e  %08x\n", expected, java.lang.Float.floatToIntBits(expected));

            printf("Actual  : ")
            print_bits(actual)
            printf("    %15e  %08x\n", actual, java.lang.Float.floatToIntBits(actual));

            false
        }
        else{
            if (verbose) printf("Match!\n")
            true
        }
    }

    def main(args: Array[String]): Unit = {
        SimConfig.withWave.compile(new FpxxDemo).doSim { dut =>
            val oscClkPeriod = 10;  // 10 ns

            val clockDomain = ClockDomain(dut.io.osc_clk)
            val cdFork = clockDomain.forkStimulus(period = oscClkPeriod)

            dut.io.op_vld #= false

            clockDomain.waitSampling()

            println("Start...")

            println("\n\nTesting LeadingZeros...\n\n")

            var pass = 0
            var fail = 0

            var rand = new scala.util.Random(0)
            var i=0
            while(i<1000){
                var lz_in : Long = (rand.nextLong & 0x00000000007fffff) >> (rand.nextInt & 0x1f)

                dut.io.lz_in #= lz_in
                clockDomain.waitSampling(3)

                var lz_exp = countLeadingZeros(lz_in)
                val lz_act = dut.io.lz.toInt

                if (lz_exp != lz_act){
                    printf("%6d: %6x: Expected: %2d, Actual: %2d - %s\n", i, lz_in, lz_exp, lz_act, if (lz_exp == lz_act) "Pass" else "Fail")
                    fail += 1
                }
                else {
                    pass += 1
                }
                i+=1
            }
            printf("LeadingZeros: %d PASSED, %d FAILED\n", pass, fail);

            println("\n\nTesting FpAdd...\n\n")

            val stimuli = Array[(Float, Float)](
                                (0,0), (0,1), (1,0),
                                (1,1), (1, -1), (-1, 1), (-1, -1),
                                (100, 1), (-100, 1), (100, -1), (1, -100), (-1, 100), (-100, -1), (-1, -100),
                                (100000000, 1), (1, 100000000),
                                (100, 0.001f), (100, -0.001f),
                                (100, -99.9999f)
                            )

            pass = 0
            fail = 0

            i=0
            while(i < stimuli.size || i < 100000){
                var inputs : (Float, Float) = (0.0f, 0.0f)
                if (i < stimuli.size){
                    inputs = stimuli(i)
                }
                else{
                    inputs = ( randomNormalFloat(rand), randomNormalFloat(rand) )
                }

                var op_a  = inputs._1
                var op_b  = inputs._2
                var sum_exp = op_a + op_b

                // Convert signed int to positive long
                var op_a_long : Long = java.lang.Float.floatToIntBits(op_a) & 0x00000000ffffffffL
                var op_b_long : Long = java.lang.Float.floatToIntBits(op_b) & 0x00000000ffffffffL

                // Apply operands
                dut.io.op_vld #= true
                dut.io.op_a   #= op_a_long
                dut.io.op_b   #= op_b_long
                clockDomain.waitSampling(1)
                dut.io.op_vld #= false

                // Wait until result appears
                while(!dut.io.op_a_p_op_b_vld.toBoolean){
                    clockDomain.waitSampling()
                }

                var sum_act = java.lang.Float.intBitsToFloat(dut.io.op_a_p_op_b.toLong.toInt)

                if (resultMatches(op_a, op_b, sum_exp, sum_act)){
                    pass += 1
                }
                else {
                    fail += 1
                    printf("%6d: %10e, %10e\n", i, op_a, op_b)
                    printf("Expected: %10e, Actual: %10e\n", sum_exp, sum_act);
                    printf("--------\n")
                    simFailure("ABORTING!")
                }

                if (i%1000 == 0) printf(".")

                i+=1
            }
            dut.io.op_vld #= false

            clockDomain.waitSampling(10)
            println("Done!")
            printf("%d PASSED, %d FAILED\n", pass, fail);
        }
    }
}
