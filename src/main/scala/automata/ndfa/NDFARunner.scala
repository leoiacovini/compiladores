package automata.ndfa

object NDFARunner {
  def fromNDFA[Input, State](ndfa: NonDeterministicFiniteAutomata[Input, State]): NDFARunner[Input, State] = {
    NDFARunner(ndfa, ndfa.initialStates)
  }
}
case class NDFARunner[Input, State](ndfa: NonDeterministicFiniteAutomata[Input, State], currentStates: Seq[State]) {
  def run(input: Input): Seq[State] = {
    for {
      state <- currentStates
      newStates <- ndfa.transition(state, input)
    } yield newStates
  }

  def withStates(newStates: Seq[State]): NDFARunner[Input, State] = copy(currentStates = newStates)
  def isAccepted: Boolean = {
    currentStates.exists(ndfa.acceptStates.contains)
  }
}
