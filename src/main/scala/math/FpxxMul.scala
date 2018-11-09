
package math

import spinal.core._

case class FpxxMulConfig(
    pipeStages      : Int = 1
    ){
}

class FpxxMul(c: FpxxConfig, mulConfig: FpxxMulConfig = null) extends Component {

    def pipeStages      = if (mulConfig == null) 1 else mulConfig.pipeStages

    val io = new Bundle {
        val op_vld      = in(Bool)
        val op_a        = in(Fpxx(c))
        val op_b        = in(Fpxx(c))

        val result_vld  = out(Bool)
        val result      = out(Fpxx(c))
    }

    val p0_vld  = io.op_vld
    val op_a_p0 = io.op_a
    val op_b_p0 = io.op_b

    val op_is_nan_p0    = op_a_p0.is_nan() || op_b_p0.is_nan()
    val op_a_is_zero_p0 = op_a_p0.is_zero()
    val op_b_is_zero_p0 = op_b_p0.is_zero()
    val op_is_zero_p0   = (op_a_is_zero_p0 || op_b_is_zero_p0) && !op_is_nan_p0

    val exp_a_p0 = op_a_p0.exp
    val exp_b_p0 = op_b_p0.exp

    val mant_a_p0 = op_a_p0.mant
    val mant_b_p0 = op_b_p0.mant

    val sign_mul_p0 = op_a_p0.sign ^ op_b_p0.sign

    //============================================================

    val p1_pipe_ena = pipeStages >= 2
    val p1_vld          = OptPipeInit(p0_vld, False, p1_pipe_ena)
    val op_is_zero_p1   = OptPipe(op_is_zero_p0,   p0_vld, p1_pipe_ena)
    val op_is_nan_p1    = OptPipe(op_is_nan_p0,    p0_vld, p1_pipe_ena)
    val sign_mul_p1     = OptPipe(sign_mul_p0,     p0_vld, p1_pipe_ena)
    val exp_a_p1        = OptPipe(exp_a_p0,        p0_vld, p1_pipe_ena)
    val exp_b_p1        = OptPipe(exp_b_p0,        p0_vld, p1_pipe_ena)
    val mant_a_p1       = OptPipe(mant_a_p0,       p0_vld, p1_pipe_ena)
    val mant_b_p1       = OptPipe(mant_b_p0,       p0_vld, p1_pipe_ena)

    //============================================================

    val exp_mul_p1 = SInt(c.exp_size+2 bits)

    exp_mul_p1 := exp_a_p1.resize(c.exp_size+2).asSInt + exp_b_p1.resize(c.exp_size+2).asSInt - S(c.bias, c.exp_size+2 bits)

    val mant_a_full_p1 = U(1, 1 bits) @@ mant_a_p1
    val mant_b_full_p1 = U(1, 1 bits) @@ mant_b_p1

    val mant_mul_p1 = UInt(c.mant_size+2 bits)
    mant_mul_p1 := (mant_a_full_p1 * mant_b_full_p1) >> c.mant_size

    //============================================================
    val p2_pipe_ena = pipeStages >= 1
    val p2_vld          = OptPipeInit(p1_vld, False, p2_pipe_ena)
    val op_is_zero_p2   = OptPipe(op_is_zero_p1,   p1_vld, p2_pipe_ena)
    val op_is_nan_p2    = OptPipe(op_is_nan_p1,    p1_vld, p2_pipe_ena)
    val sign_mul_p2     = OptPipe(sign_mul_p1,     p1_vld, p2_pipe_ena)
    val exp_mul_p2      = OptPipe(exp_mul_p1,      p1_vld, p2_pipe_ena)
    val mant_mul_p2     = OptPipe(mant_mul_p1,     p1_vld, p2_pipe_ena)
    //============================================================

    val exp_mul_adj_p2  = SInt(c.exp_size+2 bits)
    val mant_mul_adj_p2 = UInt(c.mant_size+1 bits)

    mant_mul_adj_p2 := (mant_mul_p2 |>> mant_mul_p2.msb.asUInt).resize(c.mant_size+1)
    exp_mul_adj_p2  := exp_mul_p2 + mant_mul_p2.msb.asUInt.resize(2).asSInt

    val sign_final_p2 = Bool
    val exp_final_p2  = UInt(c.exp_size bits)
    val mant_final_p2 = UInt(c.mant_size bits)

    when(op_is_nan_p2){
        sign_final_p2   := False
        exp_final_p2.setAll
        mant_final_p2   := (c.mant_size-1 -> True, default -> False)
    }
    .elsewhen(op_is_zero_p2 || exp_mul_adj_p2 <= 0){
        sign_final_p2   := False
        exp_final_p2.clearAll
        mant_final_p2.clearAll
    }
    .elsewhen(exp_mul_adj_p2 >= 255){
        sign_final_p2   := sign_mul_p2
        exp_final_p2.setAll
        mant_final_p2.clearAll
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
