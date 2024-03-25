
package math

import org.scalatest.funsuite.AnyFunSuite

import math.FpxxTesterSupport._

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim.FlowDriver
import spinal.lib.sim.FlowMonitor

object FpxxMulTester {

    class FpxxMulDut(inConfig: FpxxConfig, outConfig: FpxxConfig) extends Component {
        val io = new Bundle {
            val op = slave Flow(new Bundle {
                val a = Bits(inConfig.full_size bits)
                val b = Bits(inConfig.full_size bits)
            })

            val result      = master Flow(Bits(outConfig.full_size bits))
        }

        val fp_op = new FpxxMul(inConfig, Some(outConfig), mulConfig = FpxxMulConfig(pipeStages = 2))
        fp_op.io.input.valid :=    RegNext(io.op.valid) init(False)
        fp_op.io.input.payload.a.assignFromBits(RegNext(io.op.payload.a))
        fp_op.io.input.payload.b.assignFromBits(RegNext(io.op.payload.b))

        io.result.valid := RegNext(fp_op.io.result.valid) init(False)
        io.result.payload     := RegNext(fp_op.io.result.payload).asBits
    }
}

class FpxxMulTester extends AnyFunSuite {

    def resultMatches(opA: Float, opB: Float, expected: Float, actual: Float, verbose: Boolean = false) : Boolean = {

        val actualMant   : Long = Fp32.mant(actual)
        val expectedMant : Long = Fp32.mant(expected)

        var matches = false
        matches |= Fp32.isDenormal(expected) && Fp32.isZero(actual)
        matches |= Fp32.isInfinite(expected) && Fp32.isInfinite(actual)
        matches |= Fp32.isNaN(expected)      && Fp32.isNaN(actual)
        matches |= Fp32.isZero(expected)     && Fp32.isZero(actual)
        matches |= (Fp32.exp(expected)  == Fp32.exp(actual))  &&
                   (Fp32.sign(expected) == Fp32.sign(actual)) &&
                   ((Fp32.mant(expected) - Fp32.mant(actual)).abs < 2)

        if (!matches){
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

    test("FpxxMul") {

        val config = FpxxConfig(8, 23)

        var compiled = SimConfig
//            .withWave
            .allOptimisation
            .compile(new FpxxMulTester.FpxxMulDut(config, config))

        compiled.doSim { dut =>

            dut.clockDomain.forkStimulus(period = 10)
            dut.clockDomain.forkSimSpeedPrinter(0.2)
            dut.io.op.valid #= false
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
                val result_exp  = op_a * op_b

                // Convert signed int to positive long
                var op_a_long : Long = Fp32.asBits(op_a)
                var op_b_long : Long = Fp32.asBits(op_b)

                // Apply operands
                dut.io.op.valid #= true
                dut.io.op.a   #= op_a_long
                dut.io.op.b   #= op_b_long
                dut.clockDomain.waitSampling(1)
                dut.io.op.valid #= false

                // Wait until result appears
                while(!dut.io.result.valid.toBoolean){
                    dut.clockDomain.waitSampling()
                }

                // Actual result
                val result_act = Fp32.asFloat(dut.io.result.payload.toLong.toInt)

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

    test("float8e5m2_fnuz -> e8 -> bfloat16 multiplication") {
        val inConfig = FpxxConfig.float8_e5m2fnuz()
        val outConfig = FpxxConfig.bfloat16()

        SimConfig.withFstWave.compile(new Module {
            val input = slave Flow(new Bundle {
                val a = Fpxx(inConfig)
                val b = Fpxx(inConfig)
            })
            val result = master Flow(Fpxx(outConfig))

            val aConv = FpxxConverter(inConfig, FpxxConfig(8, 2))
            val bConv = FpxxConverter(inConfig, FpxxConfig(8, 2))
            aConv.io.a.payload := input.a
            aConv.io.a.valid := True
            bConv.io.a.payload := input.b
            bConv.io.a.valid := True

            val mult = FpxxMul(FpxxConfig(8, 2), Some(outConfig), mulConfig = FpxxMulConfig(pipeStages = 0))
            mult.io.input.payload.a := aConv.io.r
            mult.io.input.payload.b := bConv.io.r
            mult.io.input.valid := input.valid

            mult.io.result >> result
        }).doSim{dut =>
            SimTimeout(10000000)
            val scoreboard = ScoreboardInOrder[FpxxHost]
            val source = scala.io.Source.fromFile("testcases/mul_float8_e5m2fnuz_to_bfloat16.txt")
            val cases = source.getLines().map(l => l.split(" ")).map(_.toList).map {
                case a :: b :: expected :: _ => {
                    val (aB, bB, eB) = (BigInt(a, 16), BigInt(b, 16), BigInt(expected, 16))
                    (FpxxHost(aB, inConfig), FpxxHost(bB, inConfig), FpxxHost(eB, outConfig))
                }
            }

            FlowDriver(dut.input, dut.clockDomain) { payload =>
                if (!cases.isEmpty) {
                    val (a, b, expected) = cases.next()
                    payload.a #= a
                    payload.b #= b
                    scoreboard.pushRef(expected, (a, b))
                    true
                } else false
            }

            FlowMonitor(dut.result, dut.clockDomain) { payload =>
                scoreboard.pushDut(payload.toHost())
            }

            dut.clockDomain.forkStimulus(2)
            dut.clockDomain.waitActiveEdgeWhere(cases.isEmpty && scoreboard.ref.isEmpty)
        }
    }

}
