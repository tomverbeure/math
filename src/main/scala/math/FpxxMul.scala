
package math

import spinal.core._

class FpxxMul(c: FpxxConfig, pipeStages: Int = 1) extends Component {

    val io = new Bundle {
        val op_vld      = in(Bool)
        val op_a        = in(Fpxx(c))
        val op_b        = in(Fpxx(c))

        val result_vld  = out(Bool)
        val result      = out(Fpxx(c))
    }

    def optPipe[T <: Data](that : T, ena: Bool, pipeline : Boolean) : T = if (pipeline) RegNextWhen(that, ena) else that
    def optPipe[T <: Data](that : T, pipeline : Boolean) : T = optPipe(that, True, pipeline)

    val p0_vld  = io.op_vld
    val op_a_p0 = io.op_a
    val op_b_p0 = io.op_b

    val op_a_is_zero_p0 = op_a_p0.is_zero()
    val op_b_is_zero_p0 = op_b_p0.is_zero()
    val op_is_zero_p0   = op_a_is_zero_p0 || op_b_is_zero_p0

    val exp_a_p0 = op_a_p0.exp
    val exp_b_p0 = op_b_p0.exp

    val mant_a_p0 = op_a_p0.mant
    val mant_b_p0 = op_b_p0.mant

    val sign_mul_p0 = op_a_p0.sign ^ op_b_p0.sign

    //============================================================

    val p1_pipe_ena = pipeStages >= 2
    val p1_vld          = optPipe(p0_vld, p1_pipe_ena)
    val op_is_zero_p1   = optPipe(op_is_zero_p0,   p0_vld, p1_pipe_ena)
    val sign_mul_p1     = optPipe(sign_mul_p0,     p0_vld, p1_pipe_ena)
    val exp_a_p1        = optPipe(exp_a_p0,        p0_vld, p1_pipe_ena)
    val exp_b_p1        = optPipe(exp_b_p0,        p0_vld, p1_pipe_ena)
    val mant_a_p1       = optPipe(mant_a_p0,       p0_vld, p1_pipe_ena)
    val mant_b_p1       = optPipe(mant_b_p0,       p0_vld, p1_pipe_ena)

    //============================================================

    val exp_mul_p1 = SInt(c.exp_size+1 bits)

    exp_mul_p1 := exp_a_p1.resize(c.exp_size+1).asSInt + exp_b_p1.resize(c.exp_size+1).asSInt + S(1 - c.bias, c.exp_size+1 bits)

    val mant_a_full_p1 = U(1, 1 bits) @@ mant_a_p1
    val mant_b_full_p1 = U(1, 1 bits) @@ mant_b_p1

    val mant_mul_p1 = UInt(c.mant_size+1 bits)
    mant_mul_p1 := (mant_a_p1 * mant_b_p1) >> (c.mant_size+1)

    //============================================================
    val p2_pipe_ena = pipeStages >= 1
    val p2_vld          = optPipe(p1_vld, p2_pipe_ena)
    val op_is_zero_p2   = optPipe(op_is_zero_p1,   p1_vld, p2_pipe_ena)
    val sign_mul_p2     = optPipe(sign_mul_p1,     p1_vld, p2_pipe_ena)
    val exp_mul_p2      = optPipe(exp_mul_p1,      p1_vld, p2_pipe_ena)
    val mant_mul_p2     = optPipe(mant_mul_p1,     p1_vld, p2_pipe_ena)
    //============================================================

    val exp_mul_adj_p2  = SInt(c.exp_size+1 bits)
    val mant_mul_adj_p2 = UInt(c.mant_size+1 bits)

    mant_mul_adj_p2 := mant_mul_p2 >> mant_mul_p2.msb.asUInt
    exp_mul_adj_p2  := exp_mul_p2 + mant_mul_p2.msb.asSInt

    val sign_final_p2 = Bool
    val exp_final_p2  = UInt(c.exp_size bits)
    val mant_final_p2 = UInt(c.mant_size bits)

    when(op_is_zero_p2 || exp_mul_p2 < 0){
        sign_final_p2   := False
        exp_final_p2    := 0
        mant_final_p2   := 0
    }
    .otherwise{
        sign_final_p2   := sign_mul_p2
        exp_final_p2    := exp_mul_adj_p2.resize(c.exp_size).asUInt
        mant_final_p2   := mant_mul_adj_p2.resize(c.mant_size)
    }

    io.result_vld   := p2_vld
    io.result.sign  := sign_final_p2
    io.result.exp   := exp_final_p2
    io.result.mant  := mant_final_p2

}
