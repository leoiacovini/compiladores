package event_machine

case class Event[Input, Output, State](input: Input, output: Output, state: State)

case class EventResult[Output, State](output: Output, state: State)

object EventMachine {
  type ConsumeFn[Input, Output, State] = Event[Input, Output, State] => EventResult[Output, State]
}

case class EventMachine[Input, Output, State]
(initialState: State,
 initialOutput: Output,
 consumeFn: EventMachine.ConsumeFn[Input, Output, State]) {

  def consume(inputs: Seq[Input]): Output = {
    inputs.foldLeft(EventResult(initialOutput, initialState)) { (eventResult, input) =>
      consumeFn(Event(input, eventResult.output, initialState))
    }.output
  }
}
