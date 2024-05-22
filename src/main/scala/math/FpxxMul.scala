package math

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._

case class FpxxMulConfig(
    pipeStages: Int = 1,
    hwMul: Boolean = false,
    rounding: RoundType = RoundType.ROUNDTOEVEN
) {}

case class FpxxMul(cIn: FpxxConfig, cOut: Option[FpxxConfig] = None, mulConfig: FpxxMulConfig = FpxxMulConfig())
    extends Component {

    val cOutU      = cOut getOrElse cIn
    def pipeStages = mulConfig.pipeStages
    def hwMul      = mulConfig.hwMul

    assert(0 <= pipeStages && pipeStages <= 2, "Multiplier supports 0, 1 or 2 stage pipeline")
    assert(cIn.ieee_like && cOutU.ieee_like, "Can only handle IEEE compliant floats")
    assert(cIn.exp_size == cOutU.exp_size, "Can only handle equal input and output exponents")

    val io = new Bundle {
        val input = slave Flow (new Bundle {
            val a = Fpxx(cIn)
            val b = Fpxx(cIn)
        })
        val result = master Flow (Fpxx(cOutU))
    }

    val n0 = new Node {
        arbitrateFrom(io.input)

        val a = insert(io.input.payload.a)
        val b = insert(io.input.payload.b)
        val is_nan = insert(
          a.is_nan() || b.is_nan() || a.is_zero() && b.is_infinite() || b.is_zero() && a.is_infinite()
        )
        val is_inf    = insert(a.is_infinite() || b.is_infinite())
        val a_is_zero = insert(a.is_zero() || a.is_subnormal())
        val b_is_zero = insert(b.is_zero() || b.is_subnormal())
        val is_zero   = insert(a_is_zero || b_is_zero)

        val mant_a   = insert(U(1, 1 bits) @@ a.mant)
        val mant_b   = insert(U(1, 1 bits) @@ b.mant)
        val sign_mul = insert(a.sign ^ b.sign)
    }

    val n1 = new Node {
        val exp_mul  = insert((n0.a.exp +^ n0.b.exp).intoSInt - cIn.bias)
        val mant_mul = insert(n0.mant_a * n0.mant_b)
    }

    val n2 = new Node {
        arbitrateTo(io.result)

        val mant_mul_adj =
            ((n1.mant_mul @@ U(0, 1 bit)) |>> n1.mant_mul.msb.asUInt)(0, n1.mant_mul.getBitsWidth + 1 - 2 bits)

        val mant_mul_rounded =
            mant_mul_adj.fixTo(mant_mul_adj.getWidth downto mant_mul_adj.getWidth - cOutU.mant_size, mulConfig.rounding)

        val exp_mul_adj = n1.exp_mul + n1.mant_mul.msb.asUInt.intoSInt + mant_mul_rounded.msb.asUInt.intoSInt

        val result = io.result.payload

        result.sign := n0.sign_mul
        when(n0.is_nan) {
            result.set_nan()
        }.elsewhen(n0.is_inf) {
            result.set_inf()
        }.elsewhen(n0.is_zero || exp_mul_adj <= 0) {
            result.set_zero()
        }.elsewhen(exp_mul_adj >= result.exp.maxValue) {
            result.set_inf()
        }.otherwise {
            result.exp  := exp_mul_adj.asUInt.resized
            result.mant := mant_mul_rounded.resized
        }
    }

    val c01 = if (pipeStages >= 2) StageLink(n0, n1) else DirectLink(n0, n1)
    val c12 = if (pipeStages >= 1) StageLink(n1, n2) else DirectLink(n1, n2)
    Builder(c01, c12)
}
