package event_machine

import event_machine.EventMachine._

object EventMachine {

  case class Event[Input, State](input: Input, state: State)

  case class EventResult[Output, State](output: Output, state: State)

  type ConsumeFn[Input, Output, State] = Event[Input, State] => EventResult[Output, State]
}

case class EventMachine[Input, Output, State](initialState: State,
                                              consumeFn: ConsumeFn[Input, Output, State]) {

  def consume(inputs: Seq[Input]): Seq[Output] = {
    val initialResult = (Seq[Output](), initialState)
    inputs.foldLeft(initialResult) { case ((output, state), input) =>
      val result = consumeFn(Event(input, state))
      (output :+ result.output, result.state)
    }._1
  }
}
