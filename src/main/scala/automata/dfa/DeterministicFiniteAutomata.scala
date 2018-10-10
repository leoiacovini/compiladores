package automata.dfa

trait DeterministicFiniteAutomata[Symbol, +State] {
  val alphabet: Seq[Symbol]
  val initialState: State
  val states: Seq[State]
  val acceptStates: Seq[State]
  def accepts[S >: State](state: S): Boolean = acceptStates.contains(state)
  def transition[S >: State](state: S, symbol: Symbol): State
}
