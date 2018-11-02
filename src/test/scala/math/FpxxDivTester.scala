
package math

import org.scalatest.FunSuite

import math.FpxxTesterSupport._

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
        fp_op.io.op_vld :=    RegNext(io.op_vld) init(False)
        fp_op.io.op_a.fromVec(RegNext(io.op_a))
        fp_op.io.op_b.fromVec(RegNext(io.op_b))

        io.result_vld := RegNext(fp_op.io.result_vld) init(False)
        io.result     := RegNext(fp_op.io.result).toVec()
    }
}

class FpxxDivTester extends FunSuite {

    def resultMatches(opA: Float, opB: Float, expected: Float, actual: Float, verbose: Boolean = false) : Boolean = {

        val actualMant   : Long = Fp32.mant(actual)
        val expectedMant : Long = Fp32.mant(expected)

        val error_ratio = ((actual.toDouble-expected.toDouble)/actual.toDouble).abs

        var matches = false
        matches |= Fp32.isDenormal(expected) && Fp32.isZero(actual)
        matches |= Fp32.isInfinite(expected) && Fp32.isInfinite(actual)
        matches |= Fp32.isNaN(expected)      && Fp32.isNaN(actual)
        matches |= (Fp32.exp(expected)  == Fp32.exp(actual))  &&
                   (Fp32.sign(expected) == Fp32.sign(actual)) &&
                   ((Fp32.mant(expected) - Fp32.mant(actual)).abs < 3)

        matches |= (error_ratio <= (2.0/(1<<24)))

        if (!matches){
            printf("\n")
            printf("ERROR!\n")
            printAll(opA, opB, expected, actual);
            printf("Error ratio: %e\n", error_ratio)

            false
        }
        else{
            if (verbose){
                printf("Match!\n")
                printAll(opA, opB, expected, actual);
            }
            true
        }
    }

    test("FpxxDiv") {

        val config = FpxxConfig(8, 23)

        var compiled = SimConfig
//            .withWave
            .compile(new FpxxDivTester.FpxxDivDut(config))

        compiled.doSim { dut =>

            dut.clockDomain.forkStimulus(period = 10)
            dut.clockDomain.forkSimSpeedPrinter(0.2)
            dut.io.op_vld #= false
            dut.clockDomain.waitSampling()

            val stimuli = FpxxTesterSupport.directedStimuli

            var rand = new scala.util.Random(0)
            var i = 0
            var pass = 0
            var fail = 0

            while(i < stimuli.size || i < 1000000) {
                var inputs : (Float, Float) = (0.0f, 0.0f)
                if (i < stimuli.size){
                    inputs = stimuli(i)
                }
                else{
                    inputs = ( Fp32.randomRegular(rand), Fp32.randomRegular(rand) )
                }


                val op_a        = inputs._1
                val op_b        = inputs._2
                val result_exp  = op_a / op_b

                // Convert signed int to positive long
                var op_a_long : Long = Fp32.asBits(op_a)
                var op_b_long : Long = Fp32.asBits(op_b)

                // Apply operands
                dut.io.op_vld #= true
                dut.io.op_a   #= op_a_long
                dut.io.op_b   #= op_b_long
                dut.clockDomain.waitSampling(1)
                dut.io.op_vld #= false

                // Wait until result appears
                while(!dut.io.result_vld.toBoolean){
                    dut.clockDomain.waitSampling()
                }

                // Actual result
                val result_act = Fp32.asFloat(dut.io.result.toLong.toInt)

                dut.clockDomain.waitSampling()

                if (resultMatches(op_a, op_b, result_exp, result_act, verbose = false)){
                    pass += 1
                }
                else {
                    fail += 1
                    printf("%6d: %10e, %10e\n", i, op_a, op_b)
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
