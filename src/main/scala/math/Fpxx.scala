
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

class FpxxAdd(c: FpxxConfig, pipeStages: Int = 1) extends Component {

    val io = new Bundle {
        val op_a    = in(Fpxx(c))
        val op_b    = in(Fpxx(c))
        val result  = out(Fpxx(c))
    }

    val op_a_p0 = io.op_a
    val op_b_p0 = io.op_b

    val mant_a_p0 = op_a_p0.full_mant()
    val mant_b_p0 = op_b_p0.full_mant()

    val exp_add_p0 = UInt(c.exp_size bits)

    val exp_diff_a_b_p0 = SInt(c.exp_size+1 bits)
    val exp_diff_b_a_p0 = UInt(c.exp_size   bits)

    exp_diff_a_b_p0 := op_a_p0.exp.resize(c.exp_size+1).asSInt - op_b_p0.exp.resize(c.exp_size+1).asSInt
    exp_diff_b_a_p0 := op_b_p0.exp - op_a_p0.exp

    val mant_a_adj_p0 = UInt(c.mant_size+2 bits)
    val mant_b_adj_p0 = UInt(c.mant_size+2 bits)

    when(exp_diff_a_b_p0 >=0){
        exp_add_p0     := op_a_p0.exp
        mant_a_adj_p0  := mant_a_p0.resize(c.mant_size+2);
        mant_b_adj_p0  := ((exp_diff_a_b_p0 > c.mant_size) ? U(0, c.mant_size+1 bits) | mant_b_p0 |>> exp_diff_a_b_p0.resize(log2Up(c.mant_size)).asUInt ).resize(c.mant_size+2)
    }
    .otherwise{
        exp_add_p0     := op_b_p0.exp
        mant_a_adj_p0  := ((exp_diff_b_a_p0 > c.mant_size) ? U(0, c.mant_size+1 bits) | mant_a_p0 |>> exp_diff_b_a_p0.resize(log2Up(c.mant_size)) ).resize(c.mant_size+2)
        mant_b_adj_p0  := mant_b_p0.resize(c.mant_size+2);
    }

    //============================================================

    val op_a_sign_p1  = Bool
    val op_b_sign_p1  = Bool
    val exp_add_p1    = UInt(c.exp_size bits)
    val mant_a_adj_p1 = UInt(mant_a_adj_p0.getWidth bits)
    val mant_b_adj_p1 = UInt(mant_b_adj_p0.getWidth bits)

    if (pipeStages >= 1){
        op_a_sign_p1  := RegNext(op_a_p0.sign)
        op_b_sign_p1  := RegNext(op_b_p0.sign)
        exp_add_p1    := RegNext(exp_add_p0)
        mant_a_adj_p1 := RegNext(mant_a_adj_p0)
        mant_b_adj_p1 := RegNext(mant_b_adj_p0)
    }
    else{
        op_a_sign_p1  := op_a_p0.sign
        op_b_sign_p1  := op_b_p0.sign
        exp_add_p1    := exp_add_p0
        mant_a_adj_p1 := mant_a_adj_p0
        mant_b_adj_p1 := mant_b_adj_p0
    }

    //============================================================

    val sign_add_p1 = Bool
    val mant_add_p1 = UInt(c.mant_size+2 bits)

    when(op_a_sign_p1 === op_b_sign_p1){
        sign_add_p1 := op_a_sign_p1
        mant_add_p1 := mant_a_adj_p1 + mant_b_adj_p1
    }
    .elsewhen(mant_a_adj_p1 > mant_b_adj_p1){
        sign_add_p1 := op_a_sign_p1
        mant_add_p1 := mant_a_adj_p1 - mant_b_adj_p1
    }
    .otherwise{
        sign_add_p1 := op_b_sign_p1
        mant_add_p1 := mant_b_adj_p1 - mant_a_adj_p1
    }

    //============================================================

    val sign_add_p2 = Bool
    val exp_add_p2  = UInt(c.exp_size bits)
    val mant_add_p2 = UInt(mant_add_p1.getWidth bits)

    if (pipeStages >= 2){
        sign_add_p2 := RegNext(sign_add_p1)
        exp_add_p2  := RegNext(exp_add_p1)
        mant_add_p2 := RegNext(mant_add_p1)
    }
    else{
        sign_add_p2 := sign_add_p1
        exp_add_p2  := exp_add_p1
        mant_add_p2 := mant_add_p1
    }

    //============================================================

    val mant_final_p2  = UInt(c.mant_size+2 bits)
    val exp_final_p2   = UInt(c.exp_size bits)

    val lz_p2 = LeadingZeros(mant_add_p2.asBits)

    when(mant_add_p2(c.mant_size+1)){
        mant_final_p2  := mant_add_p2 |>> 1
        exp_final_p2   := exp_add_p2 + 1
    }
    .otherwise{
        mant_final_p2  := mant_add_p2 |<< lz_p2
        exp_final_p2   := exp_add_p2 - lz_p2
    }

    io.result := op_a_p0
    when(op_a_p0.is_zero()){
        io.result := op_b_p0
    }
    .elsewhen(op_b_p0.is_zero()){
        io.result := op_a_p0
    }
    .otherwise{
        io.result.sign  := sign_add_p2
        io.result.exp   := exp_final_p2
        io.result.mant  := mant_final_p2.resize(c.mant_size)
    }

}

