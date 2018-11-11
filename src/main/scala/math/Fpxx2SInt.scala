
package math

import spinal.core._

class Fpxx2SInt(intNrBits: Int, fracNrBits: Int, c: FpxxConfig) extends Component {
    
    assert(intNrBits + fracNrBits > c.mant_size, "Not enough its for SInt size")

    def pipeStages = 1

    val io = new Bundle {
        val op_vld      = in(Bool)
        val op          = in(Fpxx(c))

        val result_vld  = out(Bool)
        val result      = out(SInt((intNrBits) bits))
        val result_ovfl = out(Bool)
    }

    val p0_vld  = io.op_vld
    val op_p0   = io.op

    val sign_p0     = op_p0.sign
    val ge0_p0      = op_p0.exp >= c.bias

    val shift_p0    = SInt(c.exp_size+2 bits)
    shift_p0 := (U"00" @@ op_p0.exp).asSInt - c.bias - intNrBits - 1

    val mant_full_p0 = (U"01" @@ op_p0.mant).asSInt
    val mant_2c_p0   = sign_p0 ? -mant_full_p0 | mant_full_p0

    //============================================================
    val p1_pipe_ena = pipeStages >= 1
    val p1_vld          = OptPipeInit(p0_vld, False,   p1_pipe_ena)
    val ge0_p1          = OptPipe(ge0_p0,     p0_vld, p1_pipe_ena)
    val shift_p1        = OptPipe(shift_p0,   p0_vld, p1_pipe_ena)
    val mant_2c_p1      = OptPipe(mant_2c_p0, p0_vld, p1_pipe_ena)
    //============================================================

    val shift_clipped_p1 = UInt(log2Up(intNrBits) bits)
    shift_clipped_p1 := shift_p1.asUInt.resize(shift_clipped_p1.getWidth)

    val int_p1 = SInt(intNrBits bits)
    val ovfl_p1 = Bool

    when(shift_p1 >= intNrBits){
        int_p1.clearAll
        ovfl_p1 := False
    }
    .elsewhen(shift_p1 < 0){
        int_p1.setAll
        ovfl_p1 := True
    }
    .otherwise{
        int_p1 := (mant_2c_p1 @@ S(0, (intNrBits-mant_2c_p1.getWidth) bits)) |>> shift_clipped_p1
        ovfl_p1 := False
    }

    io.result_vld   := p1_vld
    io.result       := int_p1
    io.result_ovfl  := ovfl_p1
}

