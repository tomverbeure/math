
package math

import spinal.sim._
import spinal.core._
import spinal.core.sim._

object FpxxDemoTests {

    def randomRegularFloat(rand: scala.util.Random) : Float = {
        var ai : Int = 0
        var af : Float = 0.0f
        do {
            ai = rand.nextInt
            af = java.lang.Float.intBitsToFloat(ai)
        } while(!Fp32.isRegular(af))

        af
    }

    def printAll(opA: Float, opB: Float, expected: Float, actual: Float) {
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

    def resultMatches(opA: Float, opB: Float, expected: Float, actual: Float, verbose: Boolean = false) : Boolean = {

        val actualMant   : Long = Fp32.mant(actual)
        val expectedMant : Long = Fp32.mant(expected)

        if ((actualMant-expectedMant).abs > 15 && Fp32.isRegular(expected)){
            printf("\n")
            printf("ERROR!\n")
            printAll(opA, opB, expected, actual);

            false
        }
        else{
            if (verbose) printf("Match!\n")
            true
        }
    }

    def main(args: Array[String]): Unit = {
        SimConfig.
//            withWave.
            compile(new FpxxDemo).doSim { dut =>

            val oscClkPeriod = 10;  // 10 ns

            val clockDomain = ClockDomain(dut.io.osc_clk)
            val cdFork = clockDomain.forkStimulus(period = oscClkPeriod)

            dut.io.op_vld #= false

            clockDomain.waitSampling()

            println("Start...")

            var pass = 0
            var fail = 0

            var rand = new scala.util.Random(0)

            var i=0

            println("\n\nTesting FpAdd...\n\n")

            val stimuli = Array[(Float, Float)](
                                (0,0), (0,1), (1,0),
                                (1,1), (1, -1), (-1, 1), (-1, -1),
                                (100, 1), (-100, 1), (100, -1), (1, -100), (-1, 100), (-100, -1), (-1, -100),
                                (100000000, 1), (1, 100000000),
                                (100, 0.001f), (100, -0.001f),
                                (100, -99.9999f)
                            )

            var resetCntr = 0
            while(resetCntr < 100){
                clockDomain.waitSampling()
                resetCntr += 1
            }

            pass = 0
            fail = 0

            i=0
            while(i < stimuli.size || i < 1000000){
                var inputs : (Float, Float) = (0.0f, 0.0f)
                if (i < stimuli.size){
                    inputs = stimuli(i)
                }
                else{
                    inputs = ( randomRegularFloat(rand), randomRegularFloat(rand) )
                }

                var op_a  = inputs._1
                var op_b  = inputs._2
                var sum_exp = op_a + op_b

                // Convert signed int to positive long
                var op_a_long : Long = Fp32.asBits(op_a)
                var op_b_long : Long = Fp32.asBits(op_b)

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
                    if (false){
                        printAll(op_a, op_b, sum_exp, sum_act);
                    }
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
