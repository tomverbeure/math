
package math

import spinal.core._
import spinal.core.sim._

import scala.collection.mutable
import spinal.lib.sim.Phase

object Fp32 {

    def exp_bits    = 8
    def exp_mask    = (1L<<exp_bits)-1
    def mant_bits   = 23
    def mant_mask   = (1L<<mant_bits)-1
    def bias        = 127

    def asBits(f: Float) : Long  = java.lang.Float.floatToIntBits(f) & 0x00000000ffffffffL
    def asFloat(i: Int)  : Float = java.lang.Float.intBitsToFloat(i)

    def sign(f : Float) = asBits(f) >> (exp_bits + mant_bits)
    def exp(f : Float)  = (asBits(f) >> mant_bits) & exp_mask
    def mant(f : Float) = asBits(f) & mant_mask

    def isDenormal(f : Float) : Boolean = {
        exp(f) == 0 && mant(f) != 0
    }

    def isZero(f : Float) : Boolean = {
        exp(f) == 0 && mant(f) == 0
    }

    def isNaN(f : Float) : Boolean = {
        f.isNaN()
    }

    def isInfinite(f : Float) : Boolean = {
        f.isInfinite()
    }

    def isRegular(f : Float) : Boolean = {
        !isInfinite(f) && !isNaN(f) && !isDenormal(f)
    }


    def print_bits(f: Float) = {

        printf("%d ", sign(f))

        var i=exp_bits-1
        while(i>=0){
            printf("%d", (exp(f)>>i)&1)
            i-=1
        }

        printf(" ")
        i=mant_bits-1
        while(i>=0){
            printf("%d", (mant(f)>>i)&1)
            i-=1
        }
    }

    def print(f: Float) {
        Fp32.print_bits(f)
        printf("    %15e  %08x", f, Fp32.asBits(f));
    }

    def randomRegular(rand: scala.util.Random) : Float = {
        var ai : Int = 0
        var af : Float = 0.0f
        do {
            ai = rand.nextInt
            af = java.lang.Float.intBitsToFloat(ai)
        } while(!Fp32.isRegular(af))

        af
    }

}

object Fp64 {

    def exp_bits    = 11
    def exp_mask    = (1L<<exp_bits)-1
    def mant_bits   = 52
    def mant_mask   = (1L<<mant_bits)-1
    def bias        = 1023

    def asBits(f: Double) : Long = java.lang.Double.doubleToLongBits(f)

    def sign(f : Double) = asBits(f) >> (exp_bits + mant_bits)
    def exp(f : Double)  = (asBits(f) >> mant_bits) & exp_mask
    def mant(f : Double) = asBits(f) & mant_mask

    def isRegular(f : Double) : Boolean = {
        !f.isInfinite() && !f.isNaN() && !isDenormal(f)
    }

    def isDenormal(f : Double) : Boolean = {
        exp(f) == 0 && mant(f) != 0
    }
}

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
    def apply(mantissa: UInt, lsb: Int): (Bool, UInt) = {
        val roundUp = mantissa(lsb)
        val rounding_term = if (lsb > 0) 1 << (lsb - 1) else 0
        val term_bits = lsb bits
        val rounded_mantissa = mantissa +^ roundUp.mux(U(rounding_term.max(0), term_bits), U((rounding_term - 1).max(0), term_bits))

        (rounded_mantissa(rounded_mantissa.getWidth - 1),
            rounded_mantissa(rounded_mantissa.getWidth - 2 downto lsb))
    }
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
