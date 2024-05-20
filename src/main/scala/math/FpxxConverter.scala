package math

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._

/* Converts from one Fpxx format to another
 */
case class FpxxConverter(
    in_config: FpxxConfig,
    out_config: FpxxConfig,
    pipeStages: (Boolean, Boolean, Boolean, Boolean) = (false, false, false, false),
    rounding: RoundType = RoundType.ROUNDTOEVEN
) extends Module {
    val io = new Bundle {
        val a = slave Flow (Fpxx(in_config))
        val r = master Flow (Fpxx(out_config))
    }

    val n0 = new Node {
        arbitrateFrom(io.a)
        val a = insert(io.a.payload)
        // If the result exponent is too small to hold a's denormals, then we don't need to measure the
        // input's leading zeroes
        val a_lz         = insert(if (io.a.c.exp_size <= io.r.c.exp_size) LeadingZeros(io.a.mant.asBits) else U(0))
        val is_subnormal = insert(io.a.is_subnormal())
        val flags        = insert(io.a.flags())
        val is_zero      = insert(io.a.is_zero())
    }

    val n1 = new Node {
        val centered_exponent = insert(
          n0.is_subnormal
              .mux(-n0.a_lz.intoSInt, n0.a.exp.intoSInt)
              .resize(io.r.c.exp_size.max(io.a.c.exp_size) + 1) + (io.r.c.bias - io.a.c.bias)
        )

        val norm_mant = insert((n0.is_subnormal).mux(n0.a.mant |<< (n0.a_lz + 1), n0.a.mant))
    }

    val n2 = new Node {
        // Shift the mantissa if necessary in order to create a denormal
        val _shifted_mant =
            if (
              io.r.c.exp_size < io.a.c.exp_size ||
              (io.a.c.exp_size == io.r.c.exp_size && io.a.c.bias != io.r.c.bias)
            ) {
                val shift_amount = (-(n1.centered_exponent - 1).min(S(0))).asUInt
                // Need to keep the whole thing in order for rounding to work (sticky bit)
                ((B(1, 1 bits) ## n1.norm_mant ## B(0, io.r.c.mant_size + 1 bits)) >>
                    shift_amount).resize(n1.norm_mant.getWidth + io.r.c.mant_size + 1 bits).asBits.asUInt
            } else {
                this(n1.norm_mant)
            }

        val shifted_mant = insert(_shifted_mant)
    }

    val n3 = new Node {
        // If the output mantissa is smaller than the input mantissa, then we need to round
        val _rounded_mantissa = if (io.a.c.mant_size > io.r.c.mant_size) {
            n2.shifted_mant.fixTo(
              this(n2.shifted_mant).getWidth downto this(n2.shifted_mant).getWidth - io.r.c.mant_size,
              rounding
            )
        } else {
            U(0, 1 bit) ## n1.norm_mant ## U(0, io.r.c.mant_size - io.a.c.mant_size bits)
        }
        val carry_bit        = insert(_rounded_mantissa.msb)
        val rounded_mantissa = insert(_rounded_mantissa(0, _rounded_mantissa.getWidth - 1 bits))
    }

    val max_exponent = (1 << io.r.c.exp_size) - 1

    val min_exponent = 1 - io.r.c.mant_size

    val n4 = new Node {
        val exponent_rounded = n1.centered_exponent +^ n3.carry_bit.asUInt.intoSInt

        io.r.sign := n0.flags.sign
        when(n0.flags.nan) {
            io.r.set_nan()
        }.elsewhen(
          n0.flags.inf || exponent_rounded > io.r.exp.maxValue ||
              (exponent_rounded === io.r.exp.maxValue && Bool(io.r.c.inf_encoding.isInstanceOf[IEEEInfinity]))
        ) {
            io.r.set_inf()
        }.elsewhen(n0.is_zero) {
            io.r.set_zero()
        }.otherwise {
            val mant = n3.rounded_mantissa.asBits.asUInt
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

        arbitrateTo(io.r)
    }
    val c01 = if (pipeStages._1) StageLink(n0, n1) else DirectLink(n0, n1)
    val c12 = if (pipeStages._2) StageLink(n1, n2) else DirectLink(n1, n2)
    val c23 = if (pipeStages._3) StageLink(n2, n3) else DirectLink(n2, n3)
    val c34 = if (pipeStages._4) StageLink(n3, n4) else DirectLink(n3, n4)

    Builder(c01, c12, c23, c34)
}
