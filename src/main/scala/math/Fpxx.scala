
package math

import spinal.core._

case class FpxxConfig(
                exp_size    : Int,
                mant_size   : Int
                ) {

    def full_size() = {
        1 + exp_size + mant_size
    }
}

object Fpxx {
    def apply(exp_size: Int, mant_size: Int) : Fpxx = Fpxx(FpxxConfig(exp_size, mant_size))
}

case class Fpxx(c: FpxxConfig) extends Bundle {
    val sign    = Bool
    val exp     = UInt(c.exp_size bits)
    val mant    = UInt(c.mant_size bits)

//    def full_size() : Int = {
//        1 + c.exp_size + c.mant_size
//    }

    def is_zero() : Bool = {
        exp === 0
    }

    def set_zero() = {
        sign    := False
        exp     := 0
        mant    := 0
    }

    def full_mant() : UInt = {
        mant.resize(c.mant_size+1) | (1<<c.mant_size)
    }

    def toVec() : Bits = {
        sign ## exp.asBits ## mant.asBits
    }

    def fromVec(vec: Bits) = {
        sign    := vec(c.exp_size + c.mant_size)
        exp     := vec(c.mant_size, c.exp_size bits).asUInt
        mant    := vec(0, c.mant_size bits).asUInt
    }

    def init() : Fpxx = {
        sign init(False)
        exp  init(0)
        mant init(0)
        this
    }
}

class FpxxAdd(c: FpxxConfig) extends Component {

    val io = new Bundle {
        val op_a    = in(Fpxx(c))
        val op_b    = in(Fpxx(c))
        val result  = out(Fpxx(c))
    }

    val op_a = io.op_a
    val op_b = io.op_b

    val mant_a = op_a.full_mant()
    val mant_b = op_b.full_mant()

    val exp_add = UInt(c.exp_size bits)

    val exp_diff_a_b = SInt(c.exp_size+1 bits)
    val exp_diff_b_a = UInt(c.exp_size   bits)

    exp_diff_a_b := op_a.exp.resize(c.exp_size+1).asSInt - op_b.exp.resize(c.exp_size+1).asSInt
    exp_diff_b_a := op_b.exp - op_a.exp

    val mant_a_adj = UInt(c.mant_size+2 bits)
    val mant_b_adj = UInt(c.mant_size+2 bits)

    when(exp_diff_a_b >=0){
        exp_add     := op_a.exp
        mant_a_adj  := mant_a.resize(c.mant_size+2);
        mant_b_adj  := ((exp_diff_a_b > c.mant_size) ? U(0, c.mant_size+1 bits) | mant_b |>> exp_diff_a_b.resize(log2Up(c.mant_size)).asUInt ).resize(c.mant_size+2)
    }
    .otherwise{
        exp_add     := op_b.exp
        mant_a_adj  := ((exp_diff_b_a > c.mant_size) ? U(0, c.mant_size+1 bits) | mant_a |>> exp_diff_b_a.resize(log2Up(c.mant_size)) ).resize(c.mant_size+2)
        mant_b_adj  := mant_b.resize(c.mant_size+2);
    }


    val sign_add = Bool
    val mant_add = UInt(c.mant_size+2 bits)

    when(op_a.sign === op_b.sign){
        sign_add    := op_a.sign
        mant_add    := mant_a_adj + mant_b_adj
    }
    .elsewhen(mant_a_adj > mant_b_adj){
        sign_add    := op_a.sign
        mant_add    := mant_a_adj - mant_b_adj
    }
    .otherwise{
        sign_add    := op_b.sign
        mant_add    := mant_b_adj - mant_a_adj
    }

    val mant_final  = UInt(c.mant_size+2 bits)
    val exp_final   = UInt(c.exp_size bits)

    val lz = LeadingZeros(mant_add.asBits)

    when(mant_add(c.mant_size+1)){
        mant_final  := mant_add |>> 1
        exp_final   := exp_add + 1
    }
    .otherwise{
        mant_final  := mant_add |<< lz
        exp_final   := exp_add - lz
    }

    io.result := op_a
    when(op_a.is_zero()){
        io.result := op_b
    }
    .elsewhen(op_b.is_zero()){
        io.result := op_a
    }
    .otherwise{
        io.result.sign  := sign_add
        io.result.exp   := exp_final
        io.result.mant  := mant_final.resize(c.mant_size)
    }

}

