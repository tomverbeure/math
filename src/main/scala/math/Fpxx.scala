
package math

import spinal.core._

case class FpxxConfig(
                ext_size    : Int, 
                mant_size   : Int
                ) {
}

object Fpxx {
    def apply(ext_size: Int, mant_size: Int) : Fpxx = Fpxx(FpxxConfig(ext_size, mant_size))
}

case class Fpxx(c: FpxxConfig) extends Bundle {
    val sign    = Bool
    val exp     = UInt(c.ext_size bits)
    val mant    = UInt(c.mant_size bits)

    def init() : Fpxx = {
        sign init(False)
        exp  init(0)
        mant init(0)
        this
    }
}

class FpxxAdd(config: FpxxConfig) extends Component {

    val io = new Bundle {
        val opA     = in(Fpxx(config))
        val opB     = in(Fpxx(config))
        val result  = out(Fpxx(config))
    }

    io.result := io.opA

}

