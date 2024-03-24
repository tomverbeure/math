package math

import org.scalatest.funsuite.AnyFunSuite

import math.FpxxTesterSupport._

import spinal.sim._
import spinal.core._
import spinal.core.sim._

object IntSqrtTester {

    class IntSqrtDut(config: FpxxConfig) extends Component {
        val io = new Bundle {
            val op_vld      = in(Bool)
            val op          = in(Bits(8 bits))

            val result_vld  = out(Bool)
            val result      = out(Bits(24 bits))
        }

        val u_int_sqrt = new IntSqrt(io.op.getWidth, io.result.getWidth)
        u_int_sqrt.io.op_vld := RegNext(io.op_vld) init(False)
        u_int_sqrt.io.op     := RegNext(io.op.asUInt) init(0)

        io.result_vld := RegNext(u_int_sqrt.io.result_vld) init(False)
        io.result     := RegNext(u_int_sqrt.io.result.asBits)
    }
}

class IntSqrtTester extends AnyFunSuite {

    test("IntSqrt") {

        val config = FpxxConfig(8, 23)

        var compiled = SimConfig
            .withWave
            .compile(new IntSqrtTester.IntSqrtDut(config))

        compiled.doSim { dut =>

            dut.clockDomain.forkStimulus(period = 10)
            dut.clockDomain.forkSimSpeedPrinter(0.2)
            dut.io.op_vld #= false
            dut.clockDomain.waitSampling()

            val stimuli = Array[Int](0, 1, 3, 4, 8, 9, 15, 16, 143, 144, 145, 180, 190, 192, 200, 220, 240, 255)

            var i = 0
            var pass = 0
            var fail = 0

            while(i < stimuli.size){
                var input = stimuli(i)

                val op            = input
                val result_exp    = scala.math.sqrt(op).toFloat

                // Apply operands
                dut.io.op_vld #= true
                dut.io.op     #= op & 0xff
                dut.clockDomain.waitSampling(1)
                dut.io.op_vld #= false

                // Wait until result appears
                while(!dut.io.result_vld.toBoolean){
                    dut.clockDomain.waitSampling()
                }

                // Actual result
                val result_act = (dut.io.result.toLong.toInt.toFloat)/(1<<20).toFloat

                dut.clockDomain.waitSampling()

                printf("input %d: result_act: %f, result_exp: %f\n", op, result_act, result_exp);

                i+=1
            }
        }
    }

}
