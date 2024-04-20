package math

import org.scalatest.funsuite.AnyFunSuite

import spinal.core._
import spinal.sim._
import spinal.core.sim._


class FpxxConverterTester extends AnyFunSuite {

  def conversionTest(
      inConfig: FpxxConfig,
      outConfig: FpxxConfig,
      testFile: scala.io.Source
  ) {
      SimConfig
          .withFstWave
          .noOptimisation
          .compile(FpxxConverter(inConfig, outConfig))
          .doSim { dut =>
              dut.clockDomain.forkStimulus(period = 10)

              val cases = testFile.getLines().map(l => l.split(" ")).map(_.toList).map {
                  case in :: expected :: _ => {
                      val (iB, eB) = (BigInt(in, 16), BigInt(expected, 16))
                      (FpxxHost(iB, inConfig), FpxxHost(eB, outConfig))
                  }
              }
              for ((in, expected) <- cases) {
                  dut.io.a #= in
                  dut.io.r #= expected
                  dut.clockDomain.waitRisingEdge()
                  val out = dut.io.r.toHost()
                  if (dut.io.r.toHost() != expected) {
                      println("in:")
                      println(in)
                      println("expected:")
                      println(expected)
                      println("out:")
                      println(out)
                      simFailure()
                  }
              }
          }
  }

    test("Convert f16 to f32") {
        conversionTest(
            FpxxConfig.float16(),
            FpxxConfig.float32(),
            scala.io.Source.fromFile("testcases/f16_to_f32.txt")
        )
  }

    test("Convert float8_e5m2fnuz to bfloat16") {
        conversionTest(
            FpxxConfig.float8_e5m2fnuz(),
            FpxxConfig.bfloat16(),
            scala.io.Source.fromFile("testcases/float8_e5m2fnuz_to_bfloat16.txt")
        )
    }

    test("Convert f32 to f16") {
        conversionTest(
            FpxxConfig.float32(),
            FpxxConfig.float16(),
            scala.io.Source.fromFile("testcases/f32_to_f16.txt")
        )
    }

    test("Convert bfloat16 to float8_e5m2fnuz") {
        conversionTest(
            FpxxConfig.bfloat16(),
            FpxxConfig.float8_e5m2fnuz(),
            scala.io.Source.fromFile("testcases/bfloat16_to_float8_e5m2fnuz.txt")
        )
    }
}
