package math

import org.scalatest.funsuite.AnyFunSuite

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import scala.sys.process._

class FpxxConverterTester extends AnyFunSuite {

    def conversionTest(
        inConfig: FpxxConfig,
        outConfig: FpxxConfig,
        testLines: Iterator[String]
    ) {
        SimConfig.withWave.noOptimisation
            .compile(FpxxConverter(FpxxConverter.Options(inConfig, outConfig, pipeStages = List(true, true, true, true))))
            .doSim { dut =>
                SimTimeout(1000000)
                val stimuli = FpxxTesterSupport.parseHexCases(testLines, 1, inConfig, outConfig)

                FpxxTesterSupport.testOperation(stimuli, dut.io.a, dut.io.r, dut.clockDomain)
            }
    }

    test("Convert f16 to f32") {
        conversionTest(
          FpxxConfig.float16(),
          FpxxConfig.float32(),
          FpxxTesterSupport.testfloatGen(Seq("f16_to_f32"))
        )
    }

    test("Convert float8_e5m2fnuz to bfloat16") {
        conversionTest(
          FpxxConfig.float8_e5m2fnuz(),
          FpxxConfig.bfloat16(),
          Process(Seq("python", "testgen.py", "float8_e5m2fnuz", "bfloat16", "conv")).lineStream.iterator
        )
    }

    test("Convert f32 to f16") {
        conversionTest(
          FpxxConfig.float32(),
          FpxxConfig.float16(),
          FpxxTesterSupport.testfloatGen(Seq("f32_to_f16"))
        )
    }

    test("Convert bfloat16 to float8_e5m2fnuz") {
        conversionTest(
          FpxxConfig.bfloat16(),
          FpxxConfig.float8_e5m2fnuz(),
          Process(Seq("python", "testgen.py", "bfloat16", "float8_e5m2fnuz", "conv")).lineStream.iterator
        )
    }
}
