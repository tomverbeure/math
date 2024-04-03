
package math

import org.scalatest.funsuite.AnyFunSuite

import math.FpxxTesterSupport._

import spinal.sim._
import spinal.core._
import spinal.core.sim._

object SInt2FpxxTester {

    class SInt2FpxxDut(config: FpxxConfig) extends Component {
        val io = new Bundle {
            val op_vld      = in(Bool)
            val op_a        = in(Bits(8 bits))
            val op_b        = in(Bits(28 bits))

            val result_vld  = out(Bool)
            val result_a      = out(Bits(config.full_size bits))
            val result_b      = out(Bits(config.full_size bits))
        }

        val u_sint2fpxx_8 = new SInt2Fpxx(8, config)
        u_sint2fpxx_8.io.op_vld := RegNext(io.op_vld) init(False)
        u_sint2fpxx_8.io.op     := RegNext(io.op_a.asSInt) init(0)

        io.result_vld := RegNext(u_sint2fpxx_8.io.result_vld) init(False)
        io.result_a   := RegNext(u_sint2fpxx_8.io.result).toVec()

        val u_sint2fpxx_28 = new SInt2Fpxx(28, config)
        u_sint2fpxx_28.io.op_vld := RegNext(io.op_vld) init(False)
        u_sint2fpxx_28.io.op     := RegNext(io.op_b.asSInt) init(0)

        io.result_b   := RegNext(u_sint2fpxx_28.io.result).toVec()
    }
}

class SInt2FpxxTester extends AnyFunSuite {

    test("SInt2Fpxx") {

        val config = FpxxConfig(8, 23)

        var compiled = SimConfig
            .withWave
            .compile(new SInt2FpxxTester.SInt2FpxxDut(config))

        compiled.doSim { dut =>

            dut.clockDomain.forkStimulus(period = 10)
            dut.clockDomain.forkSimSpeedPrinter(0.2)
            dut.io.op_vld #= false
            dut.clockDomain.waitSampling()

            val stimuli = Array[Int](0, 1, -1, 45, -45, 255, -255, 256, -256, 1000, -100)

            var i = 0
            var pass = 0
            var fail = 0

            while(i < stimuli.size){
                var input = stimuli(i)

                val op            = input
                val result_a_exp  = input.toFloat
                val result_b_exp  = input.toFloat

                // Apply operands
                dut.io.op_vld #= true
                dut.io.op_a   #= op & 0xff
                dut.io.op_b   #= op & 0xfffffff
                dut.clockDomain.waitSampling(1)
                dut.io.op_vld #= false

                // Wait until result appears
                while(!dut.io.result_vld.toBoolean){
                    dut.clockDomain.waitSampling()
                }

                // Actual result
                val result_a_act = Fp32.asFloat(dut.io.result_a.toLong.toInt)
                val result_b_act = Fp32.asFloat(dut.io.result_b.toLong.toInt)

                dut.clockDomain.waitSampling()

                printf("%d: %f, %f\n", op, result_a_act, result_b_act);

                i+=1
            }

        }
    }

}
