
package math

import org.scalatest.FunSuite

import math.FpxxTesterSupport._

import spinal.sim._
import spinal.core._
import spinal.core.sim._

object FpxxSqrtTester {

    class FpxxSqrtDut(config: FpxxConfig) extends Component {
        val io = new Bundle {
            val op_vld      = in(Bool)
            val op          = in(Bits(config.full_size bits))

            val result_vld  = out(Bool)
            val result      = out(Bits(config.full_size bits))
        }

        val fp_op = new FpxxSqrt(config, FpxxSqrtConfig(pipeStages = 5) )
        fp_op.io.op_vld :=    RegNext(io.op_vld) init(False)
        fp_op.io.op.fromVec(RegNext(io.op))

        io.result_vld := RegNext(fp_op.io.result_vld) init(False)
        io.result     := RegNext(fp_op.io.result).toVec()
    }
}

class FpxxSqrtTester extends FunSuite {

    def resultMatches(op: Float, expected: Float, actual: Float, verbose: Boolean = false) : Boolean = {

        val actualMant   : Long = Fp32.mant(actual)
        val expectedMant : Long = Fp32.mant(expected)

        val mantDiff = (actualMant - expectedMant).abs

        var matches = false
        matches |= Fp32.isDenormal(expected) && Fp32.isZero(actual)
        matches |= Fp32.isInfinite(expected) && Fp32.isInfinite(actual)
        matches |= Fp32.isNaN(expected)      && Fp32.isNaN(actual)
        matches |= (Fp32.exp(expected)  == Fp32.exp(actual))  &&
                   (Fp32.sign(expected) == Fp32.sign(actual)) &&
                   (mantDiff < (1<<(24/2+2)))

        if (!matches){
            printf("\n")
            printf("ERROR!\n")
            printAll(op, expected, actual)
            printf("Mant diff: %d\n", mantDiff)

            false
        }
        else{
            if (verbose){
                printf("Match!\n")
                printAll(op, expected, actual);
            }
            true
        }
    }

    test("FpxxSqrt") {

        val config = FpxxConfig(8, 23)

        var compiled = SimConfig
//            .withWave
            .compile(new FpxxSqrtTester.FpxxSqrtDut(config))

        compiled.doSim { dut =>

            dut.clockDomain.forkStimulus(period = 10)
            dut.clockDomain.forkSimSpeedPrinter(0.2)
            dut.io.op_vld #= false
            dut.clockDomain.waitSampling()

            val stimuli = Array[Float](0.0f, 0.5f, 1.0f, 1.999f, 2.0f, 100f, -1.0f, Float.NaN, Float.PositiveInfinity, Float.NegativeInfinity, Float.MaxValue )

            var rand = new scala.util.Random(0)
            var i = 0
            var pass = 0
            var fail = 0

            while(i < stimuli.size || i < 1000000) {
                var input = 0.0f
                if (i < stimuli.size){
                    input = stimuli(i)
                }
                else{
                    input = Fp32.randomRegular(rand)
                }

                val op          = input
                val result_exp  = scala.math.sqrt(op).toFloat

                // Convert signed int to positive long
                var op_long : Long = Fp32.asBits(op)

                // Apply operands
                dut.io.op_vld #= true
                dut.io.op     #= op_long
                dut.clockDomain.waitSampling(1)
                dut.io.op_vld #= false

                // Wait until result appears
                while(!dut.io.result_vld.toBoolean){
                    dut.clockDomain.waitSampling()
                }

                // Actual result
                val result_act = Fp32.asFloat(dut.io.result.toLong.toInt)

                dut.clockDomain.waitSampling()

                if (resultMatches(op, result_exp, result_act, verbose = false)){
                    pass += 1
                }
                else {
                    fail += 1
                    printf("%6d: %10e\n", i, op)
                    printf("Expected: %10e, Actual: %10e\n", result_exp, result_act);
                    printf("--------\n")
                    simFailure("ABORTING!")
                }

                if (i%1000 == 0) printf(".")
                i+=1
            }

        }
    }

}
