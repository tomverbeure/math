package math

import org.scalatest.funsuite.AnyFunSuite

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim.StreamDriver
import spinal.lib.sim.StreamMonitor
import spinal.lib.sim.StreamReadyRandomizer

class FpxxAccumTester extends AnyFunSuite {
    test("order") {
        SimConfig.withWave
            .compile(BundleDebug.fpxxDebugBits(FpxxAccum(FpxxAdd.Options(FpxxConfig.float32(), 1))))
            .doSim { dut =>
                SimTimeout(1000000)
                dut.clockDomain.forkStimulus(2)

                dut.clockDomain.waitActiveEdge(2)

                val scoreboard = ScoreboardInOrder[FpxxHost]

                val samples = (1 to 30).map { i =>
                    val nums     = (1 to i).map(_.toFloat).toList
                    val sum      = nums.sum
                    val boolList = List.fill(nums.length - 1)(false) :+ true
                    scoreboard.pushRef(FpxxHost(sum), nums)
                    (nums.zip(boolList).toList, sum)
                }.toList

                val (driver, queue) = StreamDriver.queue(dut.io.op, dut.clockDomain)
                driver.setFactor(0.95f)
                queue ++= samples
                    .map(_._1)
                    .flatten
                    .map(x =>
                        (payload: Fragment[Fpxx]) => {
                            payload.fragment #= FpxxHost(x._1)
                            payload.last #= x._2
                        }
                    )
                    .toList

                StreamMonitor(dut.io.result, dut.clockDomain) { payload =>
                    scoreboard.pushDut(payload.toHost())
                }

                StreamReadyRandomizer(dut.io.result, dut.clockDomain).setFactor(0.95f)

                dut.clockDomain.waitActiveEdgeWhere(queue.isEmpty && scoreboard.ref.isEmpty)
            }
    }

}
