package math

import org.scalatest.funsuite.AnyFunSuite

import math.FpxxTesterSupport._

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib.sim.FlowMonitor
import spinal.lib.sim.FlowDriver

class AFix2FpxxTester extends AnyFunSuite {

    val stimuli = Array[Float](0, 0.5f, 1, -1, 45, -45, 255, -255, 256, -256, 1000.5f, -100)
    val config  = FpxxConfig.float32()

    test("AFix2Fpxx Q12.2") {
        var compiled = SimConfig.withIVerilog.withWave
            .compile(BundleDebug.fpxxDebugBits(new AFix2Fpxx(24 bits, 1 bit, config)))

        compiled.doSim { dut =>
            SimTimeout(100000)
            val scoreboard = ScoreboardInOrder[FpxxHost]
            dut.clockDomain.forkStimulus(period = 10)
            dut.clockDomain.forkSimSpeedPrinter(0.2)

            val cases = stimuli.iterator

            FlowDriver(dut.io.op, dut.clockDomain) { payload =>
                if (!cases.isEmpty) {
                    val a = cases.next()
                    payload.number #= a

                    scoreboard.pushRef(a)
                    true
                } else false
            }

            FlowMonitor(dut.io.result, dut.clockDomain) { payload =>
                scoreboard.pushDut(payload.toHost())
            }

            dut.clockDomain.forkStimulus(2)
            dut.clockDomain.waitActiveEdgeWhere(cases.isEmpty && scoreboard.ref.isEmpty)
        }
    }

}
