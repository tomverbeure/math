
package math

import spinal.sim._
import spinal.core._
import spinal.core.sim._


object FpxxDemoTests {

    def main(args: Array[String]): Unit = {
        SimConfig.withWave.compile(new FpxxDemo).doSim { dut =>
            val oscClkPeriod = 10;  // 10 ns

            val clockDomain = ClockDomain(dut.io.osc_clk)
            val cdFork = clockDomain.forkStimulus(period = oscClkPeriod)
            clockDomain.waitSampling()

            println("Start...")

            val stimuli = Array[(Float, Float)](
                                (0,0), (0,1), (1,0),
                                (1,1), (1, -1), (-1, 1), (-1, -1),
                                (100, 1), (-100, 1), (100, -1), (1, -100), (-1, 100), (-100, -1), (-1, -100),
                                (100000000, 1), (1, 100000000),
                                (100, 0.001f), (100, -0.001f)
                            )

            var i=0
            while(i < stimuli.size){
                val inputs = stimuli(i)

                var op_a  = inputs._1
                var op_b  = inputs._2
                var r_exp = op_a + op_b

                // Convert signed int to positive long
                var op_a_long : Long = java.lang.Float.floatToIntBits(op_a) & 0x00000000ffffffffL
                var op_b_long : Long = java.lang.Float.floatToIntBits(op_b) & 0x00000000ffffffffL

                printf("%6d: %10f, %10f\n", i, op_a, op_b)
                dut.io.op_a #= op_a_long
                dut.io.op_b #= op_b_long
                clockDomain.waitSampling(5)

                var r_act = java.lang.Float.intBitsToFloat(dut.io.op_a_p_op_b.toLong.toInt)
                if (r_act != r_exp)
                    printf("ERROR!\n")
                else
                    printf("MATCH!\n")
                printf("Expected: %10f, Actual: %10f\n", r_exp, r_act);

                printf("--------\n")
                i+=1
            }

            clockDomain.waitSampling(10)
            println("Done!")
        }
    }
}
