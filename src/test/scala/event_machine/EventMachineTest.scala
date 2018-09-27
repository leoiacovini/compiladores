package event_machine

import org.scalatest._

class EventMachineTest extends WordSpec {
  "EventMachine" must {

    "consume should work with a simple fn" in {
      val eventMachine = EventMachine[Int, Int, Int](0, 0,
        event => EventResult(event.output + event.input, event.state + 1))
      assert(eventMachine.consume(Seq(1, 2, 3, 4)) == 10)
    }

    "consume should work for seqs" in {
      val eventMachine = EventMachine[Int, Seq[Int], Int](0, Seq(),
        event => EventResult(event.output :+ (event.input + 1) , event.state + 1))
      assert(eventMachine.consume(Seq(1, 2, 3, 4)) == Seq(2, 3, 4, 5))
    }
  }

}
