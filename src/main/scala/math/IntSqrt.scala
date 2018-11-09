
package math

import spinal.core._

class IntSqrt(c: FpxxConfig, sqrtConfig: FpxxSqrtConfig = null) extends Component {

    val io = new Bundle {
        val op_vld      = in(Bool)
        val op          = in(UInt(14 bits))

        val result_vld  = out(Bool)
        val result      = out(UInt(14 bits))
    }

    var vld = io.op_vld
    var r = (io.op(io.op.getWidth-2, 2 bits).resize(3).asSInt - 1).resize(2)
    var q = (r >= 0) ? U"1" | U"0"
    var d = io.op(0, io.op.getWidth-2 bits)

    for(k <- io.op.getWidth-2 to 0 by -1) yield {
//        val insert_reg = if (k>= io.op.getWidth-4) k%2 == 1
//                         else if (k >= io.op.getWidth/2) true
//                         else k%2 == 1

        val insert_reg = false

        var vld_next = if (insert_reg) Reg(Bool)                    else Bool
        var r_next   = if (insert_reg) Reg(SInt(r.getWidth+2 bits)) else SInt(r.getWidth+2 bits)
        var q_next   = if (insert_reg) Reg(UInt(q.getWidth+1 bits)) else UInt(q.getWidth+1 bits)

        printf("k: %d, r_next: %d, r: %d, d: %d, q: %d\n", k, r_next.getWidth, r.getWidth, d.getWidth, q.getWidth)

        vld_next := vld

        when(r >= 0){
            r_next := (r ## d(d.getWidth-2, 2 bits)).asSInt - (U"0" @@ q @@ U"01").asSInt
        }
        .otherwise{
            r_next := (r ## d(d.getWidth-2, 2 bits)).asSInt + (U"0" @@ q @@ U"11").asSInt
        }

        when(r_next >= 0){
            q_next := q @@ U"1"
        }
        .otherwise{
            q_next := q @@ U"0"
        }

        vld = vld_next
        r = r_next
        q = q_next

        if (k > io.op.getWidth/2){
            d = if (insert_reg) RegNext(d.resize(d.getWidth-2)) else d.resize(d.getWidth-2)
        }
        else {
            d = U(0, 2 bits)
        }
    }

    io.result_vld := vld
    io.result     := q
}
