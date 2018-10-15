
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

        if ((actualMant-expectedMant).abs > 2 && Fp32.isRegular(expected)){
            printf("\n")
            printf("ERROR!\n")
            printAll(opA, opB, expected, actual);

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


    test("FpxxDivDirected") {

        val config = FpxxConfig(8, 23)

        var compiled = SimConfig
            .withWave
            .compile(new FpxxDivTester.FpxxDivDut(config))

        compiled.doSim { dut =>

            dut.clockDomain.forkStimulus(period = 10)
            dut.clockDomain.forkSimSpeedPrinter(0.2)
            dut.clockDomain.waitSampling()

            val stimuli = Array[(Float, Float)](
                                (0,0), (0,1), (1,0),
                                (1,1), (1, -1), (-1, 1), (-1, -1),
                                (100, 1), (-100, 1), (100, -1), (1, -100), (-1, 100), (-100, -1), (-1, -100),
                                (100000000, 1), (1, 100000000),
                                (100, 0.001f), (100, -0.001f),
                                (100, -99.9999f)
                            )

            var i = 0
            var pass = 0
            var fail = 0

            while(i < stimuli.size) {
                val inputs = stimuli(i)

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

                if (resultMatches(op_a, op_b, result_exp, result_act, verbose = true)){
                    pass += 1
                    printf("--------\n")
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
