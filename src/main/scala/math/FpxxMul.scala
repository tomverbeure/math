
package math

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._

case class FpxxMulConfig(
    pipeStages      : Int     = 1,
    hwMul           : Boolean = false
    ){
}

class FpxxMul(c: FpxxConfig, mulConfig: FpxxMulConfig = FpxxMulConfig()) extends Component {

    def pipeStages      = mulConfig.pipeStages
    def hwMul           = mulConfig.hwMul

    assert(0 <= pipeStages && pipeStages <= 2, "Multiplier supports 0, 1 or 2 stage pipeline")

    val io = new Bundle {
        val input = slave Flow(new Bundle {
            val a = Fpxx(c)
            val b = Fpxx(c)
        })
        val result = master Flow(Fpxx(c))
    }

    val n0 = new Node {
        arbitrateFrom(io.input)

        val a = insert(io.input.payload.a)
        val b = insert(io.input.payload.b)
        val is_nan = insert(a.is_nan() || b.is_nan())
        val a_is_zero = insert(a.is_zero())
        val b_is_zero = insert(b.is_zero())
        val is_zero = insert(a_is_zero || b_is_zero)

        val mant_a = insert(U(1, 1 bits) @@ a.mant)
        val mant_b = insert(U(1, 1 bits) @@ b.mant)
        val sign_mul = insert(a.sign ^ b.sign)
    }

    val n1 = new Node {
        val exp_mul = insert(n0.a.exp.resize(c.exp_size + 2).asSInt + n0.b.exp.resize(c.exp_size+2).asSInt - S(c.bias, c.exp_size+2 bits))
        val mant_mul = insert(n0.mant_a * n0.mant_b) >> c.mant_size
    }

    val n2 = new Node {
        arbitrateTo(io.result)

        val mant_mul_adj = n1.mant_mul |>> n1.mant_mul.msb.asUInt.resize(c.mant_size+1)
        val exp_mul_adj = n1.exp_mul + n1.mant_mul.msb.asUInt.resize(2).asSInt

        val result = io.result.payload

        when(n0.is_nan) {
            result.sign := False
            result.exp.setAll
            result.mant := (c.mant_size-1 -> True, default -> False)
        }
        .elsewhen(n0.is_zero || exp_mul_adj <= 0){
            result.sign   := False
            result.exp.clearAll
            result.mant.clearAll
        }
        .elsewhen(exp_mul_adj >= 255){
            result.sign   := n0.sign_mul
            result.exp.setAll
            result.mant.clearAll
        }
        .otherwise{
            result.sign   := n0.sign_mul
            result.exp    := exp_mul_adj.resize(c.exp_size).asUInt
            result.mant   := mant_mul_adj.resize(c.mant_size)
        }
    }

    val c01 = if (pipeStages >= 2) StageLink(n0, n1) else DirectLink(n0, n1)
    val c12 = if (pipeStages >= 1) StageLink(n1, n2) else DirectLink(n1, n2)
    Builder(c01, c12)
}
