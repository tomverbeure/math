
package math

import spinal.core._

case class FpxxConfig(
                exp_size    : Int,
                mant_size   : Int
                ) {

    def full_size = 1 + exp_size + mant_size

    def bias = (1<<(exp_size-1))-1
}

object Fpxx {
    def apply(exp_size: Int, mant_size: Int) : Fpxx = Fpxx(FpxxConfig(exp_size, mant_size))
}

case class Fpxx(c: FpxxConfig) extends Bundle {
    val sign    = Bool
    val exp     = UInt(c.exp_size bits)
    val mant    = UInt(c.mant_size bits)

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

