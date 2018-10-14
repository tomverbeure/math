
package math

import org.scalatest.FunSuite

import spinal.sim._
import spinal.core._
import spinal.core.sim._

object LeadingZerosTester {

    class LeadingZerosDut(inputSize: Int) extends Component {
        val io = new Bundle {
            val lz_in       = in(Bits(inputSize bits))
            val lz          = out(UInt(log2Up(inputSize) bits))
        }

        io.lz := RegNext(LeadingZeros(io.lz_in))
    }

}

class LeadingZerosTester extends FunSuite {

    test("LeadingZeros") { 

        def countLeadingZeros(lz_in: Long, activeBits: Int) : Int = {
            var i=activeBits-1
            var nrZeros = 0
            while(i>=0 && (lz_in&(1<<i))==0){
                nrZeros += 1
                i -= 1
            }
            nrZeros
        }

        var nrInputBits = 23

        var compiled = SimConfig
            .withWave
            .compile(new LeadingZerosTester.LeadingZerosDut(nrInputBits))

        compiled.doSim { dut =>

            dut.clockDomain.forkStimulus(period = 10)
            dut.clockDomain.forkSimSpeedPrinter(0.2)

            dut.clockDomain.waitSampling()

            printf("\nTesting LeadingZeros...\n")
            printf("lz_in size: %d\n\n", dut.io.lz_in.getWidth)
            println("Start...")

            var pass = 0
            var fail = 0

            var rand = new scala.util.Random(0)
            var i=0
            while(i<1000){
                var lz_in : Long = (rand.nextLong & ((1<<nrInputBits)-1) ) >> (rand.nextInt % nrInputBits)

                dut.io.lz_in #= lz_in
                dut.clockDomain.waitSampling(3)

                var lz_exp = countLeadingZeros(lz_in, nrInputBits)
                val lz_act = dut.io.lz.toInt

                if (lz_exp != lz_act){
                    printf("%6d: %6x: Expected: %2d, Actual: %2d - %s\n", i, lz_in, lz_exp, lz_act, if (lz_exp == lz_act) "Pass" else "Fail")
                    fail += 1
                }
                else {
                    pass += 1
                }
                i+=1
            }
            printf("LeadingZeros: %d PASSED, %d FAILED\n", pass, fail);
            if (fail > 0) {
                simFailure("FAILURES seen!")
            }
        }
    }

}
