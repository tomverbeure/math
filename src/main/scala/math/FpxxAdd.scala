package math

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._

// Doesn't support: denormals and correct signed zeros

object FpxxAdd {
    import caseapp._

    case class Options(
        @HelpMessage(cli.Cli.fpxxConfigHelpMsg)
        c: FpxxConfig,
        @HelpMessage(cli.Cli.stageMaskHelpMsg(5))
        pipeStages: StageMask = 0,
        @HelpMessage("Compute sticky bit for intermediate calculations. Default=true")
        stickyBit: Boolean = true,
        @HelpMessage(cli.Cli.roundTypeHelpMsg)
        rounding: RoundType = RoundType.ROUNDTOEVEN
    ) {}
}

class FpxxAdd(o: FpxxAdd.Options) extends Component {

    assert(o.c.ieee_like, "Can only handle IEEE compliant floats")

    val round_bits    = if (o.rounding == RoundType.ROUNDTOZERO) 0 else 3
    val preround_size = o.c.mant_size + round_bits

    val io = new Bundle {
        val op = slave Flow (new Bundle {
            val a = Fpxx(o.c)
            val b = Fpxx(o.c)
        })
        val result = master Flow (Fpxx(o.c))
    }

    val n0 = new Node {
        arbitrateFrom(io.op)
        val a = insert(io.op.a)
        val b = insert(io.op.b)

        val a_is_zero = insert(a.is_zero() || a.is_subnormal())
        val b_is_zero = insert(b.is_zero() || b.is_subnormal())

        val a_is_inf = insert(a.is_infinite())
        val b_is_inf = insert(b.is_infinite())

        val is_zero = insert(a_is_zero || b_is_zero)
        val is_nan  = insert(a.is_nan() || b.is_nan || a_is_inf && b_is_inf && a.sign =/= b.sign)
        val is_inf  = insert(a_is_inf || b_is_inf)

        val mant_a = a_is_zero.mux(U(0), a.full_mant())
        val mant_b = b_is_zero.mux(U(0), b.full_mant())

        val exp_diff_a_b = a.exp.resize(o.c.exp_size + 1).asSInt - b.exp.resize(o.c.exp_size + 1).asSInt
        val exp_diff_b_a = b.exp - a.exp

        val a_geq_b = exp_diff_a_b >= 0

        val sign_a_swap   = insert(a_geq_b.mux(a.sign, b.sign))
        val sign_b_swap   = insert(a_geq_b.mux(b.sign, a.sign))
        val exp_add       = insert(a_geq_b.mux(a.exp, b.exp))
        val exp_diff_ovfl = insert(a_geq_b.mux(exp_diff_a_b > preround_size, exp_diff_b_a > preround_size))
        val exp_diff      = insert(a_geq_b.mux(exp_diff_a_b.asUInt, exp_diff_b_a).resize(log2Up(preround_size)))
        val mant_a_swap   = insert(a_geq_b.mux(mant_a, mant_b))
        val mant_b_swap   = insert(a_geq_b.mux(mant_b, mant_a))

    }

    val n1 = new Node {
        // Align mantissas
        val mant_a_adj    = insert((n0.mant_a_swap << round_bits).resize(preround_size + 2))
        val _mant_b_shift = UInt(preround_size + 2 bits)
        _mant_b_shift := ((n0.mant_b_swap << round_bits) |>> n0.exp_diff).resize(preround_size + 2)
        if (o.stickyBit) {
            _mant_b_shift.lsb := (((U(1) << (n0.exp_diff.intoSInt - round_bits + 1)
                .max(0)
                .absWithSym) - 1).resized & n0.mant_b_swap).orR
        }
        val mant_b_adj = insert(n0.exp_diff_ovfl ? U(0) | _mant_b_shift)
    }

    val n2 = new Node {
        val _sign_add                        = Bool
        val _mant_a_opt_inv, _mant_b_opt_inv = UInt(preround_size + 3 bits)

        when(n0.sign_a_swap === n0.sign_b_swap) {
            _sign_add       := n0.sign_a_swap
            _mant_a_opt_inv := n1.mant_a_adj @@ False
            _mant_b_opt_inv := n1.mant_b_adj @@ False
        }
            .elsewhen(n1.mant_a_adj >= n1.mant_b_adj) {
                _sign_add       := n0.sign_a_swap
                _mant_a_opt_inv := n1.mant_a_adj @@ True
                _mant_b_opt_inv := ~n1.mant_b_adj @@ True
            }
            .otherwise {
                _sign_add       := n0.sign_b_swap
                _mant_a_opt_inv := ~n1.mant_a_adj @@ True
                _mant_b_opt_inv := n1.mant_b_adj @@ True
            }

        val sign_add       = insert(_sign_add)
        val mant_a_opt_inv = insert(_mant_a_opt_inv)
        val mant_b_opt_inv = insert(_mant_b_opt_inv)
    }

    val n3 = new Node {
        val mant_add = insert((n2.mant_a_opt_inv + n2.mant_b_opt_inv)(1, preround_size + 2 bits))
    }

    val n4 = new Node {
        // Doing leading zeros detection on the output of the adder adds directly to the critical path, or
        // requires an additional pipeline stage. An alternative is to do leading zeros anticipation (LZA)
        // and do it in parallel with the addition, but that's not done here.
        val _lz = n0.is_zero ? U(0) | LeadingZeros(n3.mant_add.resize(preround_size + 1).asBits)

        val _exp_add_adj  = UInt(o.c.exp_size bits)
        val _mant_add_adj = UInt(preround_size + 1 bits)

        when(n3.mant_add(preround_size + 1)) {
            _mant_add_adj                     := n3.mant_add >> 1
            if (o.stickyBit) _mant_add_adj(0) := n3.mant_add(0) || n3.mant_add(1)
            _exp_add_adj                      := n0.exp_add + 1
            _lz.clearAll
        }
            .otherwise {
                _mant_add_adj := n3.mant_add.resize(preround_size + 1)
                _exp_add_adj  := n0.exp_add
            }

        val lz           = insert(_lz)
        val exp_add_adj  = insert(_exp_add_adj)
        val mant_add_adj = insert(_mant_add_adj)
    }

    val n5 = new Node {
        val sign_final    = Bool
        val exp_final     = UInt(o.c.exp_size bits)
        val mant_renormed = (n4.mant_add_adj |<< n4.lz)
        val mant_rounded = mant_renormed.fixTo(
          mant_renormed.getWidth downto round_bits,
          o.rounding
        )
        val mant_final = UInt(o.c.mant_size bits)

        val exp_add_m_lz = SInt(o.c.exp_size + 1 bits)
        exp_add_m_lz := n4.exp_add_adj.resize(o.c.exp_size + 1).asSInt - n4.lz
            .resize(o.c.exp_size + 1)
            .asSInt + mant_rounded.msb.asUInt.intoSInt

        val exp_eq_lz = n4.exp_add_adj === n4.lz

        when(n0.is_nan) {
            sign_final := False
            exp_final.setAll
            mant_final := (o.c.mant_size - 1 -> True, default -> False)
        }.elsewhen(n0.is_inf || n4.exp_add_adj.andR) {
            sign_final := n2.sign_add
            exp_final.setAll
            mant_final.clearAll
        }.otherwise {
            sign_final := n2.sign_add
            exp_final := ((n4.lz < preround_size + 1) && !exp_add_m_lz.msb) ? exp_add_m_lz.asUInt.resize(
              o.c.exp_size
            ) | 0
            mant_final := (!exp_add_m_lz.msb && !exp_eq_lz) ? mant_rounded.resized | U(0, o.c.mant_size bits)
        }

        io.result.sign := sign_final
        io.result.exp  := exp_final
        io.result.mant := mant_final

        arbitrateTo(io.result)
    }

    implicit val maskConfig = StageMask.Config(5, List(1, 3, 0, 2, 4))
    Builder(o.pipeStages(Seq(n0, n1, n2, n3, n4, n5)))
}

class FpxxSub(o: FpxxAdd.Options) extends Component {

    val io = new Bundle {
        val op = slave Flow (new Bundle {
            val a = Fpxx(o.c)
            val b = Fpxx(o.c)
        })
        val result = master Flow (Fpxx(o.c))
    }

    val op_b = Fpxx(o.c)
    op_b.sign := !io.op.b.sign
    op_b.exp  := io.op.b.exp
    op_b.mant := io.op.b.mant

    val u_add = new FpxxAdd(o)
    u_add.io.op.valid <> io.op.valid
    u_add.io.op.a <> io.op.a
    u_add.io.op.b <> op_b

    u_add.io.result <> io.result
}
