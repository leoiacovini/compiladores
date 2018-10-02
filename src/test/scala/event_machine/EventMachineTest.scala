package event_machine

import event_machine.EventMachine._
import org.scalatest._

class EventMachineTest extends WordSpec {
  "EventMachine" must {

    "consume should work with a simple fn" in {
      val eventMachine = EventMachine[Int, Int, Int](0,
        event => EventResult(Seq(event.input + 1), event.state + 1))
      assert(eventMachine.consume(Seq(1, 2, 3, 4)) == Seq(2, 3, 4, 5))
    }

  }

}
