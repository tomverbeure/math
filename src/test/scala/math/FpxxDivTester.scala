
package math

import org.scalatest.FunSuite

import spinal.sim._
import spinal.core._
import spinal.core.sim._

object FpxxDivTester {

    class FpxxDivDut(config: FpxxConfig) extends Component {
        val io = new Bundle {
            val op_vld      = in(Bool)
            val op_a        = in(Bits(config.full_size bits))
            val op_b        = in(Bits(config.full_size bits))

            val result_vld  = out(Bool)
            val result      = out(Bits(config.full_size bits))
        }

        val fp_op = new FpxxDiv(config, FpxxDivConfig(pipeStages = 5) )
        fp_op.io.op_vld :=    RegNext(io.op_vld)
        fp_op.io.op_a.fromVec(RegNext(io.op_a))
        fp_op.io.op_b.fromVec(RegNext(io.op_b))

        io.result_vld := RegNext(fp_op.io.result_vld)
        io.result     := RegNext(fp_op.io.result).toVec()
    }
}

class FpxxDivTester extends FunSuite {

/*
    var compiled: SimCompiled[FpxxDemo] = null

    test("compile") {
        compiled = SimConfig
            .allOptimisation
            .compile(new FpxxDemo())
    }
*/

    test("FpxxDiv") {

        val config = FpxxConfig(8, 23)

        var compiled = SimConfig
            .withWave
            .compile(new FpxxDivTester.FpxxDivDut(config))

        compiled.doSim { dut =>

            dut.clockDomain.forkStimulus(period = 10)
            dut.clockDomain.forkSimSpeedPrinter(0.2)

            dut.clockDomain.waitSampling()
        }
    }

}
