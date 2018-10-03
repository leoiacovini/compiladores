package automata

case class NDFARunner[Input, State](ndfa: NonDeterministicFiniteAutomata[Input, State], currentStates: Seq[State]) {
  def run(input: Input): Seq[State] = {
    for {
      state <- currentStates
      newStates <- ndfa.transition(state, input)
    } yield newStates
  }

  def isAccepted: Boolean = {
    currentStates.exists(ndfa.acceptStates.contains)
  }
}
