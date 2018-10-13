
package math

import spinal.core._

case class FpxxDivConfig(
    pipeStages      : Int = 1,
    tableSizeBits   : Int = -1,
    lutMantBits     : Int = -1
            ){
}

class FpxxDiv(c: FpxxConfig, divConfig: FpxxDivConfig = null) extends Component {

    def pipeStages      = if (divConfig == null) 0 else divConfig.pipeStages
    def halfBits        = (c.mant_size+1)/2
    def lutMantBits     = if (divConfig == null || divConfig.lutMantBits < 0) 2*halfBits+1 else divConfig.lutMantBits
    def tableSizeBits   = if (divConfig == null || divConfig.tableSizeBits < 0) halfBits else divConfig.tableSizeBits
    def tableSize       = 1<< tableSizeBits
    
    def divTableContents = for(i <- 0 until tableSize) yield {
        val fin     = 1.0 + i.toDouble / tableSize
        val fout    = 1.0 / (fin * fin)

        val fin_exp     = Fp64.exp(fin)
        val fout_exp    = Fp64.exp(fout)
        var fout_mant   = Fp64.mant(fout)

        val shift = fin_exp - fout_exp
        val round = (fout_mant >> (Fp64.mant_bits-lutMantBits+1)) & 1

        fout_mant = (fout_mant >> (Fp64.mant_bits-lutMantBits)) + round

        U( (fout_mant << 2) | (shift & 0x3), (lutMantBits+2) bits) 
    }

    val div_table = Mem(UInt(lutMantBits+2 bits), initialContent = divTableContents)

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

    //============================================================

    val yl_p0       = op_b_p0.mant(0, halfBits bits)
    val yh_p0       = (U(1, 1 bits) @@ op_b_p0.mant)(halfBits, halfBits bits)

    val yh_m_yl_p0  = (yh_p0 << halfBits) - yl_p0.resize(2*halfBits)
    val div_addr_p0 = op_b_p0.mant >> (c.mant_size-tableSizeBits)

    val exp_p0      = op_a_p0.exp.resize(c.exp_size+1).asSInt - op_b_p0.exp.resize(c.exp_size+1).asSInt
    val sign_p0     = op_a_p0.sign ^ op_b_p0.sign

    //============================================================
    val p1_pipe_ena = true          // Always true because ROM is pipelined as well.
    val p1_vld      = OptPipe(p0_vld, p1_pipe_ena)
    val yh_m_yl_p1  = OptPipe(yh_m_yl_p0, p0_vld, p1_pipe_ena)
    val mant_a_p1   = OptPipe(op_a_p0.mant, p0_vld, p1_pipe_ena)
    val exp_p1      = OptPipe(exp_p0, p0_vld, p1_pipe_ena)
    val sign_p1     = OptPipe(sign_p0, p0_vld, p1_pipe_ena)

    val div_val_p1  = div_table.readSync(div_addr_p0, p0_vld)
    //============================================================

    val recip_yh2_p1    = U(1, 1 bits) @@ div_val_p1(2, lutMantBits bits)
    val recip_shift_p1  = div_val_p1(0, 2 bits)

    //============================================================
    val p2_pipe_ena     = pipeStages >= 1
    val p2_vld          = OptPipe(p1_vld, p2_pipe_ena)
    val yh_m_yl_p2      = OptPipe(yh_m_yl_p1, p1_vld, p2_pipe_ena)
    val mant_a_p2       = OptPipe(mant_a_p1, p1_vld, p1_pipe_ena)
    val sign_p2         = OptPipe(sign_p1, p0_vld, p1_pipe_ena)
    val exp_p2          = OptPipe(exp_p1, p0_vld, p1_pipe_ena)
    val recip_yh2_p2    = OptPipe(recip_yh2_p1, p1_vld, p2_pipe_ena)
    val recip_shift_p2  = OptPipe(recip_shift_p1, p1_vld, p2_pipe_ena)
    //============================================================

    val mant_a_full_p2  = U(1, 1 bits) @@ mant_a_p2
    val x_mul_yhyl_full_p2 = mant_a_full_p2 * yh_m_yl_p2
    val xMulYhYlShift = x_mul_yhyl_full_p2.getWidth - (2*halfBits+3)
    val x_mul_yhyl_p2 = x_mul_yhyl_full_p2(xMulYhYlShift, (2*halfBits+3) bits)

    //============================================================
    val p3_pipe_ena     = pipeStages >= 1
    val p3_vld          = OptPipe(p2_vld, p3_pipe_ena)
    val x_mul_yhyl_p3   = OptPipe(x_mul_yhyl_p2, p2_vld, p3_pipe_ena)
    val recip_yh2_p3    = OptPipe(recip_yh2_p2, p2_vld, p3_pipe_ena)
    val recip_shift_p3  = OptPipe(recip_shift_p2, p2_vld, p3_pipe_ena)
    //============================================================

    // Empty stage: useful for routing from output of one multiplier to input of next multiplier

    //============================================================
    val p4_pipe_ena     = pipeStages >= 2
    val p4_vld          = OptPipe(p3_vld, p4_pipe_ena)
    val x_mul_yhyl_p4   = OptPipe(x_mul_yhyl_p3, p3_vld, p4_pipe_ena)
    val recip_yh2_p4    = OptPipe(recip_yh2_p3, p3_vld, p4_pipe_ena)
    val recip_shift_p4  = OptPipe(recip_shift_p3, p3_vld, p4_pipe_ena)
    //============================================================

    val div_full_p4 = x_mul_yhyl_p4 * recip_yh2_p4
    val divShift = div_full_p4.getWidth-(2*halfBits+2)
    val div_p4      = div_full_p4(divShift, 2*halfBits+2 bits)

    //============================================================
    val p5_pipe_ena     = pipeStages >= 2
    val p5_vld          = OptPipe(p4_vld, p5_pipe_ena)
    val div_p5          = OptPipe(div_p4, p4_vld, p5_pipe_ena)
    val recip_shift_p5  = OptPipe(recip_shift_p4, p4_vld, p5_pipe_ena)
    //============================================================


    io.result_vld   := False
    io.result.sign  := False
    io.result.exp   := 0
    io.result.mant  := 0

}
