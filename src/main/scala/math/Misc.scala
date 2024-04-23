
package math

import spinal.core._
import spinal.core.sim._

import scala.collection.mutable
import spinal.lib.sim.Phase

object LeadingZeros {

    // Calculate leading zeros. Solution is based on method described here:
    // https://electronics.stackexchange.com/questions/196914/verilog-synthesize-high-speed-leading-zero-count
    // Code by @typingArtist on SpinalHDL gitter channel: https://gitter.im/SpinalHDL/SpinalHDL?at=5bbe075e435c2a518e81dd83

    def apply(input: Bits): UInt = calcOnes(~input).resize(log2Up(input.getWidth+1))

    def calcOnes(input: Bits): UInt = input.getWidth match {
        case 0 => U""
        case 1 => input.asUInt
        case a => {
            val leftBits = 1 << (log2Up(a)-1)
            val upper = calcOnes(input.resizeLeft(leftBits))
            val lower = calcOnes(input.resize(a - leftBits)).resize(upper.getWidth)
            (upper.msb ## lower.msb).mux(
                B"11"   -> U"10" @@ upper.resize(upper.getWidth-1),
                B"10"   -> U"01" @@ lower.resize(lower.getWidth-1),
                default -> U"00" @@ upper.resize(upper.getWidth-1)
                )
        }
    }

}

object MantissaRoundToEven {
    // Rounds to even at the given LSB, removing bits after it. Also returns the carry bit if any.
    def apply(mantissa: UInt, lsb: Int): (Bool, UInt) = new Composite(mantissa) {
        val roundUp = mantissa(lsb)
        val rounding_term = if (lsb > 0) 1 << (lsb - 1) else 0
        val term_bits = lsb bits
        val rounded_mantissa =
            mantissa +^ roundUp.mux(U(rounding_term.max(0), term_bits), U((rounding_term - 1).max(0), term_bits))

        val rounded =
            (
                rounded_mantissa(rounded_mantissa.getWidth - 1),
                rounded_mantissa(rounded_mantissa.getWidth - 2 downto lsb)
            )
    }.rounded
}

object OptPipe {
    def apply[T <: Data](that : T, ena: Bool, pipeline : Boolean) : T = if (pipeline) RegNextWhen(that, ena) else that
    def apply[T <: Data](that : T, pipeline : Boolean) : T = apply(that, True, pipeline)
}

object OptPipeInit {
    def apply[T <: Data](that : T, init: T, ena: Bool, pipeline : Boolean) : T = if (pipeline) RegNextWhen(that, ena) init(init) else that
    def apply[T <: Data](that : T, init: T, pipeline : Boolean) : T = apply(that, init, True, pipeline)
}

case class ScoreboardInOrder[T]() {
    val dut = mutable.Queue[T]()
    val ref = mutable.Queue[(T, Any)]()
    var matches = 0

    if (Phase.isUsed) {
        Phase.check {
            checkEmptyness()
        }
    }

    def pushDut(that: T): Unit = {
        dut.enqueue(that)
        check()
    }

    def pushRef(that: T, inputs: Any = null): Unit = {
        ref.enqueue((that, inputs))
        check()
    }

    def compare(ref: T, dut: T) = !(ref != dut)

    def check(): Unit = {
        if (ref.nonEmpty && dut.nonEmpty) {
            val dutHead = dut.dequeue()
            val (refHead, inputs) = ref.dequeue()
            if (!compare(refHead, dutHead)) {
                println("Transaction mismatch :")
                if (inputs != null) {
                    println("Inputs :")
                    println(inputs)
                }
                println("REF :")
                println(refHead)
                println("DUT :")
                println(dutHead)
                simFailure()
            }
            matches += 1
        }
    }

    def checkEmptyness(): Unit = {
        if (dut.nonEmpty || ref.nonEmpty) {
            if (dut.nonEmpty) {
                println("Unmatched DUT transaction : \n")
                dut.foreach(d => println(d))
            }

            if (ref.nonEmpty) {
                println("Unmatched reference transaction :\n")
                ref.foreach(d => println(d))
            }
            if (Phase.isUsed) Phase.check.onEnd(simFailure()) else simFailure()
        }
    }
}

/// Credit to Andreas Wallner on the Gitter channel
object BundleDebug {
    def apply[T <: Component](top: T, transform: Data => Option[() => Data]): T = {
        top.walkComponents { c =>
            val todo = mutable.ArrayBuffer[(Data, () => Data)]()
            val done = mutable.HashSet[Data]()
            c.dslBody.walkDeclarations { ds =>
                def recurse(d: Data): Unit = {
                    d.refOwner match {
                        case null =>
                        case dd: Data =>
                            if (dd != null && !done.contains(dd)) {
                                done.add(dd)
                                transform(dd) match {
                                    case Some(f) => todo.append((dd, f))
                                    case None    =>
                                }
                                recurse(dd)
                            }
                        case _ =>
                    }
                }

                ds match {
                    case d: Data => recurse(d)
                }
            }
            c.rework {
                todo.foreach { case (dd, f) =>
                    if (dd.isNamed) {
                        val signal = f()
                        signal.setName(dd.getName() + "__debug")
                    }
                }
            }
        }
        top
    }

    /* Adds a debug signal to the DUT which contains the concatenated bits of every Fpxx signal. */
    def fpxxDebugBits[T <: Component](top: T) = apply(
      top,
      {
          case fpxx: Fpxx => Some(() => fpxx.sign ## fpxx.exp ## fpxx.mant)
          case _          => None
      }
    )
}
