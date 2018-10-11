
package math

import spinal.core._

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
