
package math

import spinal.core._

object LeadingZeros {
    def apply(input: Bits): UInt = {
        val zeros = UInt(log2Up(input.getWidth+1) bits)

        // https://electronics.stackexchange.com/questions/196914/verilog-synthesize-high-speed-leading-zero-count

        val input_padded = if ( (input.getWidth & 1) == 1) input ## True else input

        // Encode pairs: 00 -> 10 (2 zeros), 01 -> 01 (1 leading zero), 10 and 11 -> 00 (no leading zeros)
        val encoded = Bits(input_padded.getWidth bits)

        var i = 0
        while(i<input_padded.getWidth){
            encoded(i+1 downto i) := input_padded(i+1 downto i).mux(
                                        B"00" -> B"10",
                                        B"01" -> B"01",
                                        B"10" -> B"00",
                                        B"11" -> B"00")
            i = i + 2
        }

        var tree_in = Bits
        tree_in := encoded

        // Reduce tree
        var n = 2
        var tree_in_padded = Bits

        while(n <= zeros.getWidth){
            var pad_length =  (2*n - tree_in.getWidth % (2*n)) % (2*n)
            tree_in_padded = Bits((tree_in.getWidth + pad_length) bits)
            tree_in_padded.setAll
            when(True){
                tree_in_padded(tree_in.range) := tree_in
            }

            var reduced = Bits(tree_in_padded.getWidth/(2*n)*(n+1) bits)

            var i = 0
            while(i < tree_in_padded.getWidth/2/n){
                var pair  = tree_in_padded((i+1)*(2*n)-1 downto i*(2*n))
                var left  = pair(n-1+n downto n)
                var right = pair(n-1   downto 0)

                when(left.msb && right.msb){
                    reduced((i+1)*(n+1)-1 downto i*(n+1)) := (n -> True, default -> False)
                }
                .elsewhen(!left.msb){
                    reduced((i+1)*(n+1)-1 downto i*(n+1)) := B"0" ## left
                }.
                otherwise{
                    reduced((i+1)*(n+1)-1 downto i*(n+1)) := B"10" ## right(n-2 downto 0)
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

