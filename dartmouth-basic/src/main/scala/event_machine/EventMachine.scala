package event_machine

case class EventMachine[Input, Output, State](state: State, consumeFn: (Input, State) => (Seq[Output], State)) {
  def consumeMultiple(inputs: Seq[Input]): Seq[Output] = {
    inputs.foldLeft ((Seq.empty[Output], this)) { case ((outputs, eventmachine), input) =>
        consumeFn(input, eventmachine.state) match {
          case (newOutputs, newState) =>
            (outputs ++ newOutputs, eventmachine.copy(state = newState))
        }
    }._1
  }
}
