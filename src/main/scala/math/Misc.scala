
package math

import spinal.core._

object Fp32 {

    def exp_bits    = 8
    def exp_mask    = (1L<<exp_bits)-1
    def mant_bits   = 23
    def mant_mask   = (1L<<mant_bits)-1
    def bias        = 127

    def asBits(f: Float) : Long = java.lang.Float.floatToIntBits(f) & 0x00000000ffffffffL

    def sign(f : Float) = asBits(f) >> (exp_bits + mant_bits)
    def exp(f : Float)  = (asBits(f) >> mant_bits) & exp_mask
    def mant(f : Float) = asBits(f) & mant_mask

    def isRegular(f : Float) : Boolean = {
        !f.isInfinite() && !f.isNaN() && !isDenormal(f)
    }

    def isDenormal(f : Float) : Boolean = {
        exp(f) == 0 && mant(f) != 0
    }
}

object Fp64 {

    def exp_bits    = 11
    def exp_mask    = (1L<<exp_bits)-1
    def mant_bits   = 52
    def mant_mask   = (1L<<mant_bits)-1
    def bias        = 1024

    def asBits(f: Double) : Long = java.lang.Double.doubleToLongBits(f)

    def sign(f : Double) = asBits(f) >> (exp_bits + mant_bits)
    def exp(f : Double)  = asBits(f) & exp_mask
    def mant(f : Double) = asBits(f) & mant_mask

    def isRegular(f : Double) : Boolean = {
        !f.isInfinite() && !f.isNaN() && !isDenormal(f)
    }

    def isDenormal(f : Double) : Boolean = {
        exp(f) == 0 && mant(f) != 0
    }
}

object LeadingZeros {

    // Calculate leading zeros. Solution is based on method described here:
    // https://electronics.stackexchange.com/questions/196914/verilog-synthesize-high-speed-leading-zero-count
    // Code by @typingArtist on SpinalHDL gitter channel: https://gitter.im/SpinalHDL/SpinalHDL?at=5bbe075e435c2a518e81dd83

    def apply(input: Bits): UInt = calcOnes(~input).resize(log2Up(input.getWidth+1))

    def calcOnes(input: Bits): UInt = input.getWidth match {
        case 0 => U""
        case 1 => input.asUInt
        case a => {
            val leftBits = 1 << (log2Up(a)-1)
            val upper = calcOnes(input.resizeLeft(leftBits))
            val lower = calcOnes(input.resize(a - leftBits)).resize(upper.getWidth)
            (upper.msb ## lower.msb).mux(
                B"11"   -> U"10" @@ upper.resize(upper.getWidth-1),
                B"10"   -> U"01" @@ lower.resize(lower.getWidth-1),
                default -> U"00" @@ upper.resize(upper.getWidth-1)
                )
        }
    }

}

object OptPipe {

    def apply[T <: Data](that : T, ena: Bool, pipeline : Boolean) : T = if (pipeline) RegNextWhen(that, ena) else that

    def apply[T <: Data](that : T, pipeline : Boolean) : T = apply(that, True, pipeline)
}
