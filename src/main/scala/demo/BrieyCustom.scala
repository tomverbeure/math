package demo

import spinal.core._
import vexriscv.demo._


object Briey{
  def main(args: Array[String]) {
    val config = SpinalConfig()
    config.generateVerilog({
      val toplevel = new Briey(BrieyConfig.default)
      toplevel
    })
    println("DONE")
  }
}