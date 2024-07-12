package math

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._

object FpxxRSqrt {
    import caseapp._

    case class Options(
        @HelpMessage(cli.Cli.fpxxConfigHelpMsg)
        c: FpxxConfig,
        @HelpMessage(cli.Cli.stageMaskHelpMsg(1))
        pipeStages: StageMask = 0,
        @HelpMessage("Log2 number of words in the lookup table. Default = no. mantissa bits")
        tableSizeBits: Int = -1,
        @HelpMessage("Number of mantissa bits provided by lookup table. Default = no. mantissa bits / 2")
        lutMantBits: Int = -1
    )
}

case class FpxxRSqrtConfig(
    pipeStages: Int = 1,
    tableSizeBits: Int = -1,
    lutMantBits: Int = -1
) {}

case class FpxxRSqrt(o: FpxxRSqrt.Options) extends Component {

    assert(o.c.ieee_like, "Can only handle IEEE compliant floats")
    def lutMantBits   = if (o.lutMantBits < 0) o.c.mant_size else o.lutMantBits
    def tableSizeBits = if (o.tableSizeBits < 0) o.c.mant_size / 2 else o.tableSizeBits
    def tableSize     = (1 << tableSizeBits) - (1 << (tableSizeBits - 2))

    implicit val maskConfig = StageMask.Config(1, List(0))

    def rsqrtTableContents = for (i <- 0 until tableSize) yield {
        // For implicit conversion between Double and FpxxHost
        import FpxxHost._

        // Values in range (0.5, 2.0(
        val fin  = ((1L << (tableSizeBits - 2)) + i).toDouble / (1L << (tableSizeBits - 1)).toDouble;
        val fout = 1.0 / scala.math.sqrt(fin)

        val shift = if (fin.exp - fout.exp > 0) 1 else 0

        val round     = (fout.mant >> (fout.c.mant_size - lutMantBits + 1)) & 1
        val fout_mant = (fout.mant >> (fout.c.mant_size - lutMantBits)) + round

        // printf("RSqrt table: %d: %10f -> %10f : %08x, %d, %d\n", i, fin, fout, fout_mant, (fin_exp-fout_exp), shift)

        U((fout_mant << 2) | (shift & 0x3), (lutMantBits + 2) bits)
    }

    val rsqrt_table = Mem(UInt(lutMantBits + 2 bits), initialContent = rsqrtTableContents)

    val io = new Bundle {
        val op     = slave Flow (Fpxx(o.c))
        val result = master Flow (Fpxx(o.c))
    }

    val n0 = new Node {
        arbitrateFrom(io.op)

        val vld     = insert(io.op.valid)
        val op      = insert(io.op)
        val op_zero = insert(io.op.is_zero() || io.op.is_subnormal())
        val op_nan  = insert(io.op.is_nan() || io.op.sign)
        val op_inf  = insert(io.op.is_infinite())

        val exp = insert(io.op.exp.resize(o.c.exp_size + 1).asSInt - o.c.bias)

        val gt_1 = insert(!(exp).lsb)
        val rsqrt_addr = insert(
          (((U(
            1,
            1 bits
          ) @@ io.op.mant) << gt_1.asUInt) >> (o.c.mant_size + 2 - tableSizeBits)) - (1 << (tableSizeBits - 2))
        )
    }

    val n1 = new Node {
        val rsqrt_val = if (o.pipeStages(0)) {
            rsqrt_table.readSync(n0(n0.rsqrt_addr), n0(n0.vld))
        } else {
            rsqrt_table.readAsync(n0.rsqrt_addr)
        }

        val rsqrt_shift = rsqrt_val(0, 2 bits).asSInt
        val rsqrt_mant  = rsqrt_val(2, lutMantBits bits)

        val exp_adj = SInt(o.c.exp_size + 1 bits)
        exp_adj := -((n0.exp + 1) |>> 1) - rsqrt_shift + o.c.bias

        val sign_final = Bool
        val exp_final  = UInt(o.c.exp_size bits)
        val mant_final = UInt(o.c.mant_size bits)

        when(n0.op_zero) {
            // Infinity
            sign_final := this(n0.op).sign
            exp_final.setAll
            mant_final.clearAll
        } elsewhen (this(n0.op_nan)) {
            // Negative -> NaN
            sign_final := False
            exp_final.setAll
            mant_final := (o.c.mant_size - 1 -> True, default -> False)
        } elsewhen (n0.op_inf || exp_adj <= 0) {
            // Underflow
            sign_final := False
            exp_final.clearAll
            mant_final.clearAll
        } otherwise {
            sign_final := False
            exp_final  := exp_adj.asUInt.resize(o.c.exp_size)
            mant_final := rsqrt_mant << (o.c.mant_size - lutMantBits)
        }

        io.result.sign := sign_final
        io.result.exp  := exp_final
        io.result.mant := mant_final

        arbitrateTo(io.result)
    }

    Builder(o.pipeStages(Seq(n0, n1)))
}
