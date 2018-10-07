package automata

trait DeterministicFiniteAutomata[Symbol, +State] {
  val alphabet: Seq[Symbol]
  val initialState: State
  val states: Seq[State]
  val acceptStates: Seq[State]
  def accepts[S >: State](state: S): Boolean = acceptStates.contains(state)
  def transition[S >: State](state: S, symbol: Symbol): State
}

object DFARunner {
  def fromDFA[Input, State](dfa: DeterministicFiniteAutomata[Input, State]) = DFARunner(dfa, dfa.initialState)
}
case class DFARunner[Input, State](dfa: DeterministicFiniteAutomata[Input, State], currentState: State) {
  def run(input: Input): DFARunner[Input, State] = {
    withState(dfa.transition(currentState, input))
  }

  def withState(newState: State): DFARunner[Input, State] = copy(currentState = newState)
  def isAccepted: Boolean = dfa.accepts(currentState)

}
