package math

import org.scalatest.funsuite.AnyFunSuite

import math.FpxxTesterSupport._

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

object FpxxAddTester {

    case class FpxxAddDut(config: FpxxConfig) extends Component {
        val op     = slave Flow (Vec(Fpxx(config), 2))
        val result = master Flow (Fpxx(config))

        val inner = new FpxxAdd(config)
        inner.io.op.valid := op.valid
        inner.io.op.a     := op.payload(0)
        inner.io.op.b     := op.payload(1)

        result << inner.io.result
    }
}

class FpxxAddTester extends AnyFunSuite {

    test("add float16") {
        val config = FpxxConfig.float16()

        SimConfig.withWave.noOptimisation
            .compile(BundleDebug.fpxxDebugBits(FpxxAddTester.FpxxAddDut(config)))
            .doSim { dut =>
                SimTimeout(100000)
                val stimuli = parseHexCases(scala.io.Source.fromFile("testcases/f16_add.txt"), 2, config, config, false)
                    // No denormals
                    .filter { a => !a._2.isDenormal && !a._1.map(_.isDenormal).reduce(_ || _) }
                testOperation(stimuli, dut.op, dut.result, dut.clockDomain)
            }
    }

    test("add float32") {
        val config = FpxxConfig.float32()

        SimConfig.withWave.noOptimisation
            .compile(BundleDebug.fpxxDebugBits(FpxxAddTester.FpxxAddDut(config)))
            .doSim { dut =>
                SimTimeout(100000)
                val stimuli = parseHexCases(scala.io.Source.fromFile("testcases/f32_add.txt"), 2, config, config, false)
                    // No denormals
                    .filter { a => !a._2.isDenormal && !a._1.map(_.isDenormal).reduce(_ || _) }
                testOperation(stimuli, dut.op, dut.result, dut.clockDomain)
            }
    }

}
