package math

import org.scalatest.funsuite.AnyFunSuite

import math.FpxxTesterSupport._

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim.FlowDriver
import spinal.lib.sim.FlowMonitor

object FpxxMulTester extends AnyFunSuite {
    case class FpxxMulDut(config: FpxxConfig) extends Component {
        val dut = FpxxMul(config, mulConfig = FpxxMulConfig(pipeStages = 2))

        val op = slave(Flow(Vec(cloneOf(dut.io.input.payload.a), 2)))
        dut.io.input << op.map { payload =>
            val bundle = cloneOf(dut.io.input.payload)
            bundle.a := payload(0)
            bundle.b := payload(1)

            bundle
        }

        val res = master(cloneOf(dut.io.result))
        res << dut.io.result
    }
}

class FpxxMulTester extends AnyFunSuite {

    def mulTest(
        config: FpxxConfig,
        testLines: Iterator[String]
    ) {
        SimConfig.withIVerilog.withWave.noOptimisation
            .compile(BundleDebug.fpxxDebugBits(FpxxMulTester.FpxxMulDut(config)))
            .doSim { dut =>
                SimTimeout(100000)
                val stimuli = parseHexCases(testLines, 2, config, config, false)
                    // Avoid smallest value since it may involve subnormal rounding
                    .filter { a => !(a._2.mant == 0 && a._2.exp == 1) }
                    // No denormals
                    .filter { a => !a._2.isDenormal && !a._1.map(_.isDenormal).reduce(_ || _) }

                testOperation(stimuli, dut.op, dut.res, dut.clockDomain)
            }
    }

    test("float16 multiplication") {
        mulTest(
          FpxxConfig.float16(),
          testfloatGen(Seq("f16_mul"))
        )
    }

    test("float32 multiplication") {
        mulTest(
          FpxxConfig.float32(),
          testfloatGen(Seq("f32_mul"))
        )
    }

    test("float8e5m2_fnuz -> e8 -> bfloat16 multiplication") {
        val inConfig  = FpxxConfig.float8_e5m2fnuz()
        val outConfig = FpxxConfig.bfloat16()

        SimConfig.withFstWave
            .compile(BundleDebug.fpxxDebugBits(new Module {
                val input  = slave Flow (Vec(Fpxx(inConfig), 2))
                val result = master Flow (Fpxx(outConfig))

                val aConv = FpxxConverter(FpxxConverter.Options(inConfig, FpxxConfig(8, 2)))
                val bConv = FpxxConverter(FpxxConverter.Options(inConfig, FpxxConfig(8, 2)))
                aConv.io.a.payload := input.payload(0)
                aConv.io.a.valid   := True
                bConv.io.a.payload := input.payload(1)
                bConv.io.a.valid   := True

                val mult = FpxxMul(
                  FpxxConfig(8, 2),
                  Some(outConfig),
                  mulConfig = FpxxMulConfig(pipeStages = 2, rounding = RoundType.FLOOR)
                )
                mult.io.input.payload.a := aConv.io.r
                mult.io.input.payload.b := bConv.io.r
                mult.io.input.valid     := input.valid

                mult.io.result >> result
            }))
            .doSim { dut =>
                SimTimeout(10000000)
                import scala.sys.process._
                val lines =
                    Process(Seq("python", "testgen.py", "float8_e5m2fnuz", "bfloat16", "mul")).lineStream.iterator

                testOperation(
                  parseHexCases(lines, 2, FpxxConfig.float8_e5m2fnuz(), FpxxConfig.bfloat16()),
                  dut.input,
                  dut.result,
                  dut.clockDomain
                )
            }
    }

}
