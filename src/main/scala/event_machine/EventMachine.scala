package event_machine


object EventMachine {
  def consume[Input, Output, State](em: EventMachine[Input, Output, State], input: Input) = {
    val (outputs, newState) = em.consumeFn(input, em.state)
    (outputs, em.copy(state = newState))
  }
}
case class EventMachine[Input, Output, State](state: State, consumeFn: (Input, State) => (Seq[Output], State)) {
  def consumeMultiple(inputs: Seq[Input]): Seq[Output] = {
    inputs.foldLeft ((Seq.empty[Output], this)) { case ((outputs, eventMachine), input) =>
      val (newOutputs, newEventMachine) = EventMachine.consume(eventMachine, input)
      (outputs ++ newOutputs, newEventMachine)
    }._1
  }
}
