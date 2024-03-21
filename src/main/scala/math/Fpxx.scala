
package math

import spinal.core._


sealed trait InfinityEncoding {
    def isInfinity(value: Fpxx): Bool
    def assignInfinity(value: Fpxx)
};
case class NoInfinity(largest: BigInt) extends InfinityEncoding {
    def isInfinity(value: Fpxx): Bool = False
    def assignInfinity(value: Fpxx): Unit = {
        value.assignFromBits(largest, value.asBits.getWidth - 2, 0)
    }
};
case class IEEEInfinity() extends InfinityEncoding {
    def isInfinity(value: Fpxx): Bool = value.exp.andR && !value.mant.orR
    def assignInfinity(value: Fpxx): Unit = {
        value.exp.setAll()
        value.mant := U(0)
    }
};

sealed trait NanEncoding {
    def isNaN(value: Fpxx): Bool
    def assignNan(value: Fpxx)
};

case class IEEENan() extends NanEncoding {
    def isNaN(value: Fpxx): Bool = value.exp.andR && value.mant.orR
    def assignNan(value: Fpxx) = {
        value.exp.setAll()
        value.mant.setAll()
    }
};

case class SpecialNan(encoding: BigInt) extends NanEncoding {
    def isNaN(value: Fpxx): Bool = value.asBits.asUInt === U(encoding)
    def assignNan(value: Fpxx) = {
        value.assignFromBits(B(encoding))
    }
};

sealed trait Bias {
    def apply(exp_size: Int): Int
}

case class IEEEBias() extends Bias {
    def apply(exp_size: Int) = (1<<(exp_size-1))-1
}

case class CustomBias(bias: Int) extends Bias {
    def apply(exp_size: Int) = bias
}

object FpxxConfig {
    def float64() = FpxxConfig(11, 53)
    def float32() = FpxxConfig(8, 23)
    def float16() = FpxxConfig(5, 10)
    def bfloat16() = FpxxConfig(8, 7)
    def float8_e5m2fnuz() = FpxxConfig(5, 2, SpecialNan(BigInt("10000000", 2)),
        inf_encoding = NoInfinity((1 << 8) - 1), exp_bias = CustomBias(16))
}

case class FpxxConfig(
                exp_size    : Int,
                mant_size   : Int,
    		nan_encoding: NanEncoding = IEEENan(),
    		inf_encoding: InfinityEncoding = IEEEInfinity(),
    		exp_bias: Bias = IEEEBias()
) {

    def full_size = 1 + exp_size + mant_size

    def bias = exp_bias(exp_size)

    def ieee_like = nan_encoding.isInstanceOf[IEEENan] && inf_encoding.isInstanceOf[IEEEInfinity] && exp_bias.isInstanceOf[IEEEBias]
}



object Fpxx {
    def apply(exp_size: Int, mant_size: Int) : Fpxx = Fpxx(FpxxConfig(exp_size, mant_size))
}

case class Fpxx(c: FpxxConfig) extends Bundle {
    val mant    = UInt(c.mant_size bits)
    val exp     = UInt(c.exp_size bits)
    val sign    = Bool

    def is_zero() : Bool = {
        exp === 0 && mant === 0
    }

    def is_nan(): Bool = {
        c.nan_encoding.isNaN(this)
    }

    def is_infinite(): Bool = {
        c.inf_encoding.isInfinity(this)
    }

    def is_subnormal(): Bool = {
        exp === 0
    }

    def set_nan() = {
        c.nan_encoding.assignNan(this)
    }

    def set_inf() = {
        c.inf_encoding.assignInfinity(this)
    }

    def set_zero() = {
        exp     := 0
        mant    := 0
    }

    def abs() : Fpxx = {
        val abs = Fpxx(c)

        abs.sign := False
        abs.exp  := exp
        abs.mant := mant

        abs
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

    def fromDouble(d: Double) = {
        if (Fp64.exp(d) == 0){
            sign    := False
            exp     := U(0, c.exp_size bits)
            mant    := U(0, c.mant_size bits)
        }
        else{
            sign    := Bool((Fp64.sign(d) & 1) == 1)
            exp     := U(Fp64.exp(d)- Fp64.bias + c.bias, c.exp_size bits)
            mant    := U(Fp64.mant(d) >> (Fp64.mant_bits - c.mant_size), c.mant_size bits)
        }
    }

    def init() : Fpxx = {
        sign init(False)
        exp  init(0)
        mant init(0)
        this
    }
}

