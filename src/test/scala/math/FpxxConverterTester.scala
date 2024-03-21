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
          .compile(new Module {
              val input = in(Bits(inConfig.full_size bits))
              val output = out(Bits(outConfig.full_size bits))
              val expected = in(Bits(outConfig.full_size bits))

              val conv = FpxxConverter(inConfig, outConfig)
              conv.io.a.assignFromBits(input)
        
              output := conv.io.r.asBits

              val expected_fpxx = Fpxx(outConfig)
              expected_fpxx.assignFromBits(expected)

              val bothNan = out(Bool())
              bothNan := conv.io.r.is_nan && expected_fpxx.is_nan()
              val bothInf = out(Bool())
              bothInf := expected_fpxx.is_infinite && conv.io.r.is_infinite && expected_fpxx.sign === conv.io.r.sign
          })
          .doSim { dut =>
              dut.clockDomain.forkStimulus(period = 10)

              val cases = testFile.getLines().map(l => l.split(" ")).map(_.toList).map {
                  case in :: expected :: _ => {
                      (BigInt(in, 16), BigInt(expected, 16))
                  }
              }
              for ((in, expected) <- cases) {
                  dut.input #= in
                  dut.expected #= expected
                  dut.clockDomain.waitRisingEdge()
                  val out = dut.output.toBigInt
                  val inputStr = FloatHexString(in, inConfig)
                  val expectedStr = FloatHexString(expected, outConfig)
                  val outStr = FloatHexString(out, outConfig)
                  val str = s"\nin\t\tout\t\texpected\n" +
                  s"${inputStr}\t${outStr}\t${expectedStr}\n" +
                  f"${in.toString(16)}\t\t${out.toString(16)}\t\t${expected.toString(16)}"
                  assert((dut.output.toBigInt == expected) || dut.bothInf.toBoolean || dut.bothNan.toBoolean, str)
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
