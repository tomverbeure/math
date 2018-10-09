
package math

import spinal.core._

// Calculate leading zeros. Solution is based on method described here:
// https://electronics.stackexchange.com/questions/196914/verilog-synthesize-high-speed-leading-zero-count
object LeadingZeros {
    def apply(input: Bits): UInt = {
        val zeros = UInt(log2Up(input.getWidth+1) bits)

        // Initial encoding works on pairs, so optionally extend input to get even number of bits
        val input_padded = if ( (input.getWidth & 1) == 1) input ## True else input

        // Encode pairs: 00 -> 10 (2 zeros), 01 -> 01 (1 leading zero), 10 and 11 -> 00 (no leading zeros)
        val encoded = Bits(input_padded.getWidth bits)

        var i = 0
        while(i<input_padded.getWidth){
            encoded(i, 2 bits) := input_padded(i+1 downto i).mux( B"00" -> B"10", B"01" -> B"01", default -> B"00")
            i = i + 2
        }

        var tree_in = Bits
        tree_in := encoded

        // Reduce tree
        var n = 2
        while(n <= zeros.getWidth){
            // Pad input such that we always have a length that's an integer multiple of 2*n
            var pad_length =  (2*n - tree_in.getWidth % (2*n)) % (2*n)
            var tree_in_padded = Bits((tree_in.getWidth + pad_length) bits)

            if (pad_length == 0)
                tree_in_padded = tree_in
            else {
                var pad_vec = Bits(pad_length bits)
                pad_vec.setAll
                tree_in_padded = tree_in ## pad_vec
            }

            // For each adjacent pair of n-sized vectors, we create one new vector that is n+1.
            var reduced = Bits(tree_in_padded.getWidth/(2*n)*(n+1) bits)

            var i = 0
            while(i < tree_in_padded.getWidth/2/n){
                var left  = tree_in_padded(i*(2*n)+n, n bits)
                var right = tree_in_padded(i*(2*n)  , n bits)

                when(left.msb && right.msb){
                    reduced(i*(n+1), n+1 bits) := (n -> True, default -> False)
                }
                .elsewhen(!left.msb){
                    reduced(i*(n+1), n+1 bits) := B"0" ## left
                }.
                otherwise{
                    reduced(i*(n+1), n+1 bits) := B"01" ## right(n-2 downto 0)
                }
                i = i + 1
            }

            tree_in = reduced
            n += 1

            if(n > zeros.getWidth){
                zeros := reduced.resize(zeros.getWidth).asUInt
            }
        }
        zeros
    }
}

