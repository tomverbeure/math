
package math

import org.scalatest.FunSuite

import spinal.sim._
import spinal.core._
import spinal.core.sim._

object FpxxTester {

    class LeadingZerosDut extends Component {
        val io = new Bundle {
            val lz_in       = in(Bits(23 bits))
            val lz          = out(UInt(5 bits))
        }

        io.lz := RegNext(LeadingZeros(io.lz_in))
    }

    class FpxxAddDut(config: FpxxConfig) extends Component {
        val io = new Bundle {
            val op_vld      = in(Bool)
            val op_a        = in(Bits(config.full_size bits))
            val op_b        = in(Bits(config.full_size bits))

            val result_vld  = out(Bool)
            val result      = out(Bits(config.full_size bits))
        }

        val fp_op = new FpxxAdd(config, pipeStages = 5)
        fp_op.io.op_vld :=    RegNext(io.op_vld)
        fp_op.io.op_a.fromVec(RegNext(io.op_a))
        fp_op.io.op_b.fromVec(RegNext(io.op_b))

        io.result_vld := RegNext(fp_op.io.result_vld)
        io.result     := RegNext(fp_op.io.result).toVec()
    }
}

class FpxxTester extends FunSuite {

/*
    var compiled: SimCompiled[FpxxDemo] = null

    test("compile") {
        compiled = SimConfig
            .allOptimisation
            .compile(new FpxxDemo())
    }
*/

    test("LeadingZeros") { 

        def countLeadingZeros(lz_in: Long, activeBits: Int) : Int = {
            var i=activeBits-1
            var nrZeros = 0
            while(i>=0 && (lz_in&(1<<i))==0){
                nrZeros += 1
                i -= 1
            }
            nrZeros
        }

        var compiled = SimConfig
            .withWave
            .compile(new FpxxTester.LeadingZerosDut())

        compiled.doSim { dut =>

            dut.clockDomain.forkStimulus(period = 10)
            dut.clockDomain.forkSimSpeedPrinter(0.2)

            dut.clockDomain.waitSampling()

            printf("\nTesting LeadingZeros...\n")
            printf("lz_in size: %d\n\n", dut.io.lz_in.getWidth)
            println("Start...")

            var pass = 0
            var fail = 0

            var rand = new scala.util.Random(0)
            var i=0
            while(i<1000){
                var lz_in : Long = (rand.nextLong & 0x00000000007fffff) >> (rand.nextInt & 0x1f)

                dut.io.lz_in #= lz_in
                dut.clockDomain.waitSampling(3)

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

    test("FpxxAdd") {

        val config = FpxxConfig(8, 23)

        var compiled = SimConfig
            .withWave
            .compile(new FpxxTester.FpxxAddDut(config))

        compiled.doSim { dut =>

            dut.clockDomain.forkStimulus(period = 10)
            dut.clockDomain.forkSimSpeedPrinter(0.2)

            dut.clockDomain.waitSampling()
        }
    }

}
