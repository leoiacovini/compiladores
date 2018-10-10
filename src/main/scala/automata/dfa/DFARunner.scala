package automata.dfa


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
