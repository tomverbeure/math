
package math

import spinal.core._

case class FpxxDivConfig(
    pipeStages      : Int = 1,
    tableSizeBits   : Int = -1,
    lutMantBits     : Int = -1
            ){
}

class FpxxDiv(c: FpxxConfig, divConfig: FpxxDivConfig = null) extends Component {

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

    val divTable = Mem(UInt(lutMantBits bits), initialContent = divTableContents)

    val io = new Bundle {
        val op_vld      = in(Bool)
        val op_a        = in(Fpxx(c))
        val op_b        = in(Fpxx(c))

        val result_vld  = out(Bool)
        val result      = out(Fpxx(c))
    }


    io.result_vld   := False
    io.result.sign  := False
    io.result.exp   := 0
    io.result.mant  := 0

}
