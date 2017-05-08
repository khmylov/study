package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val signal1 = a1.getSignal
      val signal2 = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(signal1 | signal2) }
    }

    a1 addAction orAction
    a2 addAction orAction
  }
  
  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val a3, a4, a5 = new Wire
    inverter(a1, a3)
    inverter(a2, a4)
    andGate(a3, a4, a5)
    inverter(a5, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    (c, out) match {
      case (cHead::Nil, out1::out0::Nil) =>
        val dec = new Wire
        inverter(cHead, dec)
        andGate(in, cHead, out1)
        andGate(in, dec, out0)
      case (cHead::cTail, _) =>
        val out1, out0 = new Wire
        demux(in, List(cHead), List(out1, out0))
        val (leftPart, rightPart) = out.splitAt(out.length / 2)
        demux(out1, cTail, leftPart)
        demux(out0, cTail, rightPart)
    }
  }

  def demuxAlternative(in: Wire, c: List[Wire], out: List[Wire]) {
    def simpleDemux(input: Wire, control: Wire, out1: Wire, out0: Wire) {
      val dec = new Wire()
      inverter(control, dec)
      andGate(input, control, out1)
      andGate(input, dec, out0)
    }

    def buildRow(rowControl: Wire, rowOutputs: List[Wire], inputs: List[Wire]): List[Wire] = {
      rowOutputs match {
        case Nil => inputs.reverse
        case out1::out0::outTail =>
          val input = new Wire
          simpleDemux(input, rowControl, out1, out0)
          buildRow(rowControl, outTail, input::inputs)
      }
    }

    (c, out) match {
      //case (Nil, output::Nil) => connect(in, output)
      case (cHead::Nil, out1::out0::Nil) => simpleDemux(in, cHead, out1, out0)
      case (cHead::cTail, _) =>
        val nextRowOutputs = buildRow(cHead, out, Nil)
        demux(in, cTail, nextRowOutputs)
    }
  }

}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}
