
package math

import spinal.sim._
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable.Queue

object FpxxDemoTests {

    def main(args: Array[String]): Unit = {
        SimConfig.withWave.compile(new FpxxDemo).doSim { dut =>
            val oscClkPeriod = 10;  // 10 ns

            val clockDomain = ClockDomain(dut.io.osc_clk)
            val cdFork = clockDomain.forkStimulus(period = oscClkPeriod)

            dut.io.op_vld #= false

            clockDomain.waitSampling()

            println("Start...")

            println("\n\nTesting LeadingZeros...\n\n")

            def countLeadingZeros(lz_in: Long) : Int =
            {
                var i=22
                var nrZeros = 0
                while(i>=0 && (lz_in&(1<<i))==0){
                    nrZeros += 1
                    i -= 1
                }
                nrZeros
            }

            def print_bits(f: Float)
            {
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

            var pass = 0
            var fail = 0

            var rand = scala.util.Random
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

            var expResults = Queue[(Float, Float, Float)]()

            pass = 0
            fail = 0

            i=0
            while(i < stimuli.size){
                val inputs = stimuli(i)

                var op_a  = inputs._1
                var op_b  = inputs._2
                var r_exp = op_a + op_b

                // Convert signed int to positive long
                var op_a_long : Long = java.lang.Float.floatToIntBits(op_a) & 0x00000000ffffffffL
                var op_b_long : Long = java.lang.Float.floatToIntBits(op_b) & 0x00000000ffffffffL

                printf("%6d: %10e, %10e\n", i, op_a, op_b)
                dut.io.op_vld #= true
                dut.io.op_a   #= op_a_long
                dut.io.op_b   #= op_b_long

                expResults += ((r_exp, op_a, op_b))

                clockDomain.waitSampling(6)

                var r_act = java.lang.Float.intBitsToFloat(dut.io.op_a_p_op_b.toLong.toInt)
                printf("Expected: %10e, Actual: %10e\n", r_exp, r_act);

                if (r_act != r_exp){
                    fail += 1
                    printf("ERROR!\n")
                    printf("Expected: ");
                    print_bits(r_exp)
                    printf("\n");
                    printf("Actual  : ");
                    print_bits(r_act)
                    printf("\n");
                }
                else{
                    pass += 1
                    printf("MATCH!\n")
                }

                printf("--------\n")
                i+=1
            }
            dut.io.op_vld #= false

//            while(!expResults.isEmpty){
//                printf("Queue: %f\n", expResults.dequeue._1)
//            }

            clockDomain.waitSampling(10)
            println("Done!")
            printf("%d PASSED, %d FAILED\n", pass, fail);
        }
    }
}
