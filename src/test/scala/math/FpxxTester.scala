
package math

import org.scalatest.FunSuite

import spinal.sim._
import spinal.core._
import spinal.core.sim._

class FpxxTester extends FunSuite {

    var compiled: SimCompiled[FpxxDemo] = null

    def countLeadingZeros(lz_in: Long, activeBits: Int) : Int = {
        var i=activeBits-1
        var nrZeros = 0
        while(i>=0 && (lz_in&(1<<i))==0){
            nrZeros += 1
            i -= 1
        }
        nrZeros
    }

    test("compile") {
        compiled = SimConfig
            .allOptimisation
            .compile(new FpxxDemo())
    }

    test("LeadingZeros") { 

        compiled = SimConfig
            .withWave
            .compile(new FpxxDemo())

        compiled.doSim { dut =>

            val oscClkPeriod = 10;  // 10 ns

            val clockDomain = ClockDomain(dut.io.osc_clk)
            val cdFork = clockDomain.forkStimulus(period = oscClkPeriod)

            dut.io.op_vld #= false

            clockDomain.waitSampling()

            println("Start...")

            printf("\nTesting LeadingZeros...\n")
            printf("lz_in size: %d\n\n", dut.io.lz_in.getWidth)

            var pass = 0
            var fail = 0

            var rand = new scala.util.Random(0)
            var i=0
            while(i<1000){
                var lz_in : Long = (rand.nextLong & 0x00000000007fffff) >> (rand.nextInt & 0x1f)

                dut.io.lz_in #= lz_in
                clockDomain.waitSampling(3)

                var lz_exp = countLeadingZeros(lz_in, dut.io.lz_in.getWidth)
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
        }
    }

}
