package math.cli

import spinal.core._

import math._

object Cli {
    import caseapp._
    import caseapp.core.argparser.{ArgParser, SimpleArgParser}

    sealed trait Command;

    val fpxxConfigHelpMsg =
        "Floating point format. May be one of (float64, float32, float16, bfloat16, e5m2fnuz, e4m2fnuz, eXmX), where X is a whole number and stands for the exponent and mantissa sizes."
    implicit val fpxxConfigParser: ArgParser[FpxxConfig] =
        SimpleArgParser.from[FpxxConfig]("fpxxconfig") { s =>
            val pattern = "e(\\d+)m(\\d+)".r
            s match {
                case "float64"  => Right(FpxxConfig.float64)
                case "float32"  => Right(FpxxConfig.float32)
                case "float16"  => Right(FpxxConfig.float16)
                case "bfloat16" => Right(FpxxConfig.bfloat16)
                case "e5m2fnuz" => Right(FpxxConfig.float8_e5m2fnuz)
                case "e4m3fnuz" => Right(FpxxConfig.float8_e4m3fnuz)
                case pattern(e, m) => {
                    val config = FpxxConfig(
                      e.toInt,
                      m.toInt
                    )
                    Right(config)
                }
                case other => {
                    Left(caseapp.core.Error.Other(s"Unknown format: $other"))
                }
            }

        }

    val roundTypeHelpMsg = "Round result to: (infinity, zero, even)"
    implicit val roundTypeParser: ArgParser[spinal.core.RoundType] =
        SimpleArgParser.from("roundtype") {
            case "infinity" => Right(RoundType.ROUNDTOINF)
            case "zero"     => Right(RoundType.ROUNDTOZERO)
            case "even"     => Right(RoundType.ROUNDTOEVEN)
            case other      => Left(caseapp.core.Error.Other(s"Unknown rounding type: $other"))
        }

    implicit val modeParser: ArgParser[SpinalMode] = SimpleArgParser.from("mode") {
        case "vhdl"          => Right(VHDL)
        case "verilog"       => Right(Verilog)
        case "systemverilog" => Right(SystemVerilog)
        case other           => Left(caseapp.core.Error.Other(s"Unknown language: $other"))
    }

    def stageMaskHelpMsg(n: Int) =
        f"How many pipeline stages to use (${n} max). Can be an integer or a mask of the form: " +
            f"m${List.tabulate(n)(_ => 0).mkString}. 0 = disable, 1 = enable. Leftmost digit is the earliest stage."
    implicit val stageMaskParser: ArgParser[StageMask] = SimpleArgParser.from("stageMask") { s =>
        val mask = "m([0-1]+)".r
        val int  = "(\\d+)".r
        s match {
            case mask(m) => Right(m.toCharArray.map(_.asDigit.toBoolean).toList)
            case int(i)  => Right(i.toInt)
            case other   => Left(caseapp.core.Error.Other(s"Unknown stage mask format: $other"))
        }
    }

    case class FpxxAdd(@Recurse c: CommonOptions, @Recurse o: math.FpxxAdd.Options) extends Command {};

    case class FpxxConverter(@Recurse c: CommonOptions, @Recurse o: math.FpxxConverter.Options) extends Command {};

    case class FpxxMul(@Recurse c: CommonOptions, @Recurse o: math.FpxxMul.Options) extends Command {};

    case class FpxxAccum(@Recurse c: CommonOptions, @Recurse o: math.FpxxAdd.Options) extends Command {};

    case class FpxxRSqrt(@Recurse c: CommonOptions, @Recurse o: math.FpxxRSqrt.Options) extends Command {};

    case class CommonOptions(
        @HelpMessage("Language to generate (vhdl, verilog, systemverilog)")
        language: SpinalMode = Verilog
    ) {}

    object App extends CommandApp[Command] {

        def toConfig(options: CommonOptions) = SpinalConfig(mode = options.language, fixToWithWrap = false)

        def run(command: Command, arg: RemainingArgs): Unit = {
            command match {
                case FpxxAdd(c, o)       => toConfig(c).generate(new math.FpxxAdd(o))
                case FpxxConverter(c, o) => toConfig(c).generate(math.FpxxConverter(o))
                case FpxxMul(c, o)       => toConfig(c).generate(math.FpxxMul(o))
                case FpxxAccum(c, o)     => toConfig(c).generate(math.FpxxAccum(o))
                case FpxxRSqrt(c, o)     => toConfig(c).generate(math.FpxxRSqrt(o))
            }
        }
    }

    // The main from App doesn't seem to work so redefine it here
    def main(args: Array[String]): Unit = App.main(args)
}
