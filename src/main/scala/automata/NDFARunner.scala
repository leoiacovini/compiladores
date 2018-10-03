package automata

case class NDFARunner[Input, State](ndfa: NonDeterministicFiniteAutomata[Input, State], currentStates: Set[State]) {
  def run(input: Input): Set[State] = {
    for {
      state <- currentStates
      newStates <- ndfa.transition(state, input)
    } yield newStates
  }

  def isAccepted: Boolean = {
    currentStates.exists(ndfa.acceptStates.contains)
  }
}
