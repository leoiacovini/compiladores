package automata.ndpa

case class TransitionResult[State, StackSymbol](newState: State, stack: Seq[StackSymbol])
