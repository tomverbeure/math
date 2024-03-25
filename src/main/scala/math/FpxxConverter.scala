package math

import spinal.core._
import spinal.lib._

/* Converts from one Fpxx format to another
 */
case class FpxxConverter(
    in_config: FpxxConfig,
    out_config: FpxxConfig
) extends Module {
    val io = new Bundle {
        val a = slave Flow(Fpxx(in_config))
        val r = master Flow(Fpxx(out_config))
    }

    io.r.valid := io.a.valid

    // If the result exponent is too small to hold a's denormals, then we don't need to measure the
    // input's leading zeroes
    val a_lz = if (io.a.c.exp_size <= io.r.c.exp_size) LeadingZeros(io.a.mant.asBits) else U(0)

    val centered_exponent = io.a.is_subnormal().mux(-a_lz.intoSInt, io.a.exp.intoSInt).resize(io.r.c.exp_size.max(io.a.c.exp_size) + 1) + (io.r.c.bias - io.a.c.bias)

    val norm_mant = (io.a.is_subnormal()).mux(io.a.mant |<< (a_lz + 1), io.a.mant)

    // Shift the mantissa if necessary in order to create a denormal
    val shifted_mant = if (io.r.c.exp_size < io.a.c.exp_size ||
        (io.a.c.exp_size == io.r.c.exp_size && io.a.c.bias != io.r.c.bias)) {
        val shift_amount = (-(centered_exponent - 1).min(S(0))).asUInt
        // Need to keep the whole thing in order for rounding to work (sticky bit)
        ((B(1, 1 bits) ## norm_mant ## B(0, io.r.c.mant_size + 1 bits)) >>
            shift_amount).resize(norm_mant.getWidth + io.r.c.mant_size + 1 bits).asBits.asUInt
    } else {
        norm_mant
    }
    // If the output mantissa is smaller than the input mantissa, then we need to round
    val (carry_bit, rounded_mantissa) = if (io.a.c.mant_size > io.r.c.mant_size) {
        MantissaRoundToEven(shifted_mant, shifted_mant.getWidth - io.r.c.mant_size)
    } else {
        (False, norm_mant ## U(0, io.r.c.mant_size - io.a.c.mant_size bits))
    }

    val exponent_rounded = centered_exponent +^ carry_bit.asUInt.intoSInt

    val max_exponent = (1 << io.r.c.exp_size) - 1

    val min_exponent = 1 - io.r.c.mant_size

    io.r.sign := io.a.sign
    when(io.a.is_nan()) {
        io.r.set_nan()
    }.elsewhen(io.a.is_infinite() || exponent_rounded > io.r.exp.maxValue ||
        (exponent_rounded === io.r.exp.maxValue && Bool(io.r.c.inf_encoding.isInstanceOf[IEEEInfinity]))) {
        io.r.set_inf()
    }.elsewhen(io.a.is_zero()) {
        io.r.set_zero()
    }.otherwise {
        val mant = rounded_mantissa.asBits.asUInt
        io.r.mant := mant
        val exp = exponent_rounded.max(0).asUInt.resized
        io.r.exp := exp
        // If the format does not support signed zero, then we have to make sure we don't output it
        if (!io.r.c.signed_zero) {
            when(!mant.orR && !exp.orR) {
                io.r.sign := False
            }
        }
    }
}
