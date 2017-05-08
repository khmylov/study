package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def wiresToString(wires: List[Wire]): String = {
    wires.foldLeft("")((acc, wire) => {
      val value = wire.getSignal match { case true => "1" case false => "0" }
      acc + value
    })
  }

  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  test("orGate") {
    val in1, in2, out = new Wire

    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 2")

    in1.setSignal(false)
    in2.setSignal(true)

    assert(out.getSignal === true, "or 3")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 4")
  }

  test("orGate2") {
    val in1, in2, out = new Wire

    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 2")

    in1.setSignal(false)
    in2.setSignal(true)

    assert(out.getSignal === true, "or 3")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 4")
  }

  test("demux 1 to 2") {
    val in, c1, out1, out2 = new Wire
    demux(in, c1::Nil, out2::out1::Nil)

    def assertDemux(expectedOut1: Boolean, expectedOut2: Boolean, name: String) {
      assert(out1.getSignal === expectedOut1, name + " OUT1")
      assert(out2.getSignal === expectedOut2, name + " OUT2")
    }

    in.setSignal(false)
    c1.setSignal(false)
    run
    assertDemux(false, false, "demux1")

    c1.setSignal(true)
    run
    assertDemux(false, false, "demux2")

    in.setSignal(true)
    c1.setSignal(false)
    run
    assertDemux(true, false, "demux3")

    c1.setSignal(true)
    run
    assertDemux(false, true, "demux4")
  }

  test("demux 1 to 4") {
    val in, c1, c0, out3, out2, out1, out0 = new Wire

    def assertDemux(inputValue: Int, c1Value: Int, c0Value: Int, expectedOut: Int) {
      in.setSignal(inputValue == 1)
      c1.setSignal(c1Value == 1)
      c0.setSignal(c0Value == 1)

      run

      val testName = s"Input is $inputValue, C is $c1Value$c0Value"
      val wiresText = wiresToString(List(out3, out2, out1, out0))
      println(s"$testName, Outputs are $wiresText")

      def assertWire(wire: Wire, outNumber: Int) {
        val signal = outNumber match {
          case 0 => false
          case x => x == expectedOut
        }
        assert(wire.getSignal === signal, testName + s", Expected out #$outNumber to be $signal")
      }


      if (inputValue == 0) {
        assertWire(out3, 0)
        assertWire(out2, 0)
        assertWire(out2, 0)
        assertWire(out1, 0)
      } else {
        assertWire(out3, 4)
        assertWire(out2, 3)
        assertWire(out1, 2)
        assertWire(out0, 1)
      }
    }

    demux(in, List(c1, c0), List(out3, out2, out1, out0))

    assertDemux(0, 0, 0, 0)
    assertDemux(1, 0, 0, 1)
    assertDemux(1, 0, 1, 2)
    assertDemux(1, 1, 0, 3)
    assertDemux(1, 1, 1, 4)
  }
}
