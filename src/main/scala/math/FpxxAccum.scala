package math

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

/*
 Accumulator which works by keeping a number of sums equal to the number of pipeline stages in flight.
 Stalls for stages * log2(stages) cycles after last accumulation element is received.
 */
case class FpxxAccum(o: FpxxAdd.Options) extends Component {
    val adder = new FpxxAdd(o)

    val io = new Bundle {
        val op     = slave Stream (Fragment(Fpxx(o.c)))
        val result = master Stream (Fpxx(o.c))
    }

    val adderLatency = LatencyAnalysis(adder.io.op.valid, adder.io.result.valid)
    assert(adderLatency > 0, "Accumulator only supports pipelined adder")

    io.result.payload.setAsReg() init

    // Keep track of the number of in-flight sums in order to reduce them at the end
    val inFlight = Reg(UInt(log2Up(adderLatency + 1) bits)) init 0

    when(adder.io.op.fire && !adder.io.result.fire) {
        assert(inFlight < adderLatency.intValue() || clockDomain.isResetActive, "inFlight should not overflow")
        inFlight := inFlight + 1
    } elsewhen (!adder.io.op.fire && adder.io.result.fire) {
        assert(inFlight > 0 || clockDomain.isResetActive, "inFlight should not underflow")
        inFlight := inFlight - 1
    }

    adder.io.op.a      := io.result.payload
    adder.io.op.b.sign := False
    adder.io.op.b.set_zero()

    adder.io.op.valid := False
    io.op.ready       := False
    io.result.valid   := False
    val fsm = new StateMachine {
        val receive: State = new State with EntryPoint {
            whenIsActive {
                io.op.ready := True

                // Continually recirculate the sums since we don't use backpressure in the adder
                adder.io.op.valid := True

                when(io.op.valid) {
                    io.result.payload.set_zero()
                    adder.io.op.b := io.op.fragment
                    when(io.op.last) {
                        goto(reduce)
                    }
                }

                when(adder.io.result.valid) {
                    io.result.payload := adder.io.result
                }
            }
        }

        val reduce = new State {
            whenIsActive {
                io.op.ready       := False
                adder.io.op.b     := adder.io.result
                adder.io.op.valid := False

                when(adder.io.result.valid) {
                    // Only issue more additions when the accumulator is non-zero
                    // and there are sums in-flight
                    when(inFlight > 0 && !io.result.payload.is_zero()) {
                        adder.io.op.valid := True
                        io.result.payload.set_zero()
                    } otherwise {
                        // Else the accumulator is empty and we can set it
                        io.result.payload := adder.io.result
                    }
                }

                // We are done when the are no more sums in-flight
                io.result.valid := inFlight === 0

                when(io.result.fire) {
                    io.result.payload.set_zero()
                    goto(receive)
                }
            }
        }
    }
}
