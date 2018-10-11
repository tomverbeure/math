
package math

import spinal.core._

class FpxxDemo extends Component {
    
    val config = FpxxConfig(8, 23)

    val io = new Bundle {
        val osc_clk     = in(Bool)

        val op_vld      = in(Bool)
        val op_a        = in(Bits(config.full_size bits))
        val op_b        = in(Bits(config.full_size bits))

        val op_a_p_op_b_vld     = out(Bool)
        val op_a_p_op_b         = out(Bits(config.full_size bits))

        val lz_in       = in(Bits(23 bits))
        val lz          = out(UInt(5 bits))
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

        val add = new FpxxAdd(config, pipeStages = 5)
        add.io.op_vld :=    RegNext(io.op_vld)
        add.io.op_a.fromVec(RegNext(io.op_a))
        add.io.op_b.fromVec(RegNext(io.op_b))

        io.op_a_p_op_b_vld := RegNext(add.io.result_vld)
        io.op_a_p_op_b     := RegNext(add.io.result).toVec()

        io.lz := RegNext(LeadingZeros(io.lz_in))
    }
}

object TopVerilog {
    def main(args: Array[String]) {
        SpinalVerilog(new FpxxDemo)
    }
}


