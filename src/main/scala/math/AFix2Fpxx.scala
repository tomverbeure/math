package math

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._

class AFix2Fpxx(
    intNrBits: BitCount,
    fracNrBits: BitCount,
    c: FpxxConfig,
    pipeStages: Int = 2,
    consumeFlags: Boolean = false
) extends Component {

    assert(c.ieee_like, "Can only handle IEEE compliant floats")

    val io = new Bundle {
        val op = slave Flow (new Bundle {
            val number = AFix.SQ(intNrBits, fracNrBits)
            val flags  = consumeFlags generate Fpxx.Flags()
        })
        val result = master Flow (Fpxx(c))
    }

    val opBits = io.op.payload.number.getBitsWidth - 1

    val n0 = new Node {
        arbitrateFrom(io.op)
        val flags  = consumeFlags generate insert(io.op.flags)
        val op     = insert(io.op.number)
        val sign   = insert(op.raw.msb)
        val op_abs = insert(op.asSInt.absWithSym)
    }

    val n1 = new Node {
        val lz = insert(LeadingZeros(n0.op_abs.asBits).resize(log2Up(opBits)))
    }

    val n2 = new Node {
        val op_adj = n0.op_abs |<< n1.lz
        val result = Fpxx(c)
        result.clearAll().allowOverride()
        result.sign := n0.sign

        val baseWhen = if (consumeFlags) {
            when { n0.flags.inf } {
                result.sign := n0.flags.sign
                result.set_inf()
            }.elsewhen(n0.flags.nan) {
                result.sign := n0.flags.sign
                result.set_nan()
            }
        } else when { False }()

        baseWhen
            .elsewhen(n0.op_abs === 0) {
                result.set_zero()
            }
            .otherwise {
                result.sign := n0.sign
                result.exp  := intNrBits.value + c.bias - 1 - this(n1.lz)

                if (opBits - 1 >= c.mant_size) {
                    result.mant := op_adj(opBits - c.mant_size - 1, c.mant_size bits)
                } else {
                    result.mant := op_adj.resize(opBits - 1) @@ U(0, (c.mant_size - opBits + 1) bits)
                }
            }

        io.result.payload := result

        arbitrateTo(io.result)
    }

    val c01 = if (pipeStages >= 1) StageLink(n0, n1) else DirectLink(n0, n1)
    val c12 = if (pipeStages >= 2) StageLink(n1, n2) else DirectLink(n1, n2)
    Builder(c01, c12)
}
