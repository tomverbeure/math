
package math

import spinal.core._

class FpxxDemo extends Component {
    
    val config = FpxxConfig(8, 23)

    val io = new Bundle {
        val osc_clk     = in(Bool)

        val opA         = in(Fpxx(config))
        val opB         = in(Fpxx(config))
        val opA_p_opB   = out(Fpxx(config))
    }

    noIoPrefix()

    val resetCtrlClockDomain = ClockDomain(
        clock = io.osc_clk,
        frequency = FixedFrequency(50 MHz),
        config = ClockDomainConfig(
                    resetKind = BOOT
        )
    )

    val resetCtrl = new ClockingArea(resetCtrlClockDomain) {
        val reset_unbuffered_ = True

        val reset_cntr = Reg(UInt(5 bits)) init(0)
        when(reset_cntr =/= U(reset_cntr.range -> true)){
            reset_cntr := reset_cntr + 1
            reset_unbuffered_ := False
        }

        val reset_ = RegNext(reset_unbuffered_)
    }

    val coreClockDomain = ClockDomain(
        clock = io.osc_clk,
        reset = resetCtrl.reset_,
        config = ClockDomainConfig(
            resetKind = SYNC,
            resetActiveLevel = LOW
        )
    )

    val core = new ClockingArea(coreClockDomain) {

        val add = new FpxxAdd(config)
        add.io.opA     <> io.opA
        add.io.opB     <> io.opB
        add.io.result  <> io.opA_p_opB
    
    }
}

object TopVerilog {
    def main(args: Array[String]) {
        SpinalVerilog(new FpxxDemo)
    }
}


