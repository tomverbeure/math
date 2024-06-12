package math

import org.scalatest.funsuite.AnyFunSuite

import math.FpxxTesterSupport._

import spinal.sim._
import spinal.core._
import spinal.core.sim._

class FpxxRSqrtTester extends AnyFunSuite {
    test("32 bit") {
        val config = FpxxConfig.float32()

        SimConfig.withIVerilog.withWave.noOptimisation
            .compile(BundleDebug.fpxxDebugBits(FpxxRSqrt(FpxxRSqrt.Options(config, pipeStages = 1))))
            .doSim { dut =>
                val lines = testfloatGen(Seq("-n", "100000", "f32")).map(s => {
                    val input   = java.lang.Float.intBitsToFloat(Integer.parseUnsignedInt(s, 16))
                    val invsqrt = 1.0 / scala.math.sqrt(input)
                    f"$s ${FpxxHost(invsqrt.floatValue()).value}%x"
                })

                SimTimeout(1000000)
                val stimuli = parseHexCases(lines, 1, config, config, false, maxUlpDist = 1 << (24 / 2 + 2))
                    // No denormals
                    .filter { a => !a._2.isDenormal && !a._1.map(_.isDenormal).reduce(_ || _) }
                testOperation(stimuli, dut.io.op, dut.io.result, dut.clockDomain)
            }
    }
}
