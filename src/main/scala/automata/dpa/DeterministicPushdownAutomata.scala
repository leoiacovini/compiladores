package automata.dpa

trait DeterministicPushdownAutomata[InputSymbol, StackSymbol, +State] {
  val inputAlphabet: Seq[InputSymbol]
  val stackAlphabet: Seq[StackSymbol]
  val initialStackSymbol: StackSymbol
  val initialState: State
  val states: Seq[State]
  val acceptStates: Seq[State]
  val trapState: State
  def accepts[S >: State](state: S): Boolean = acceptStates.contains(state)
  def transition[S >: State](state: S, inputSymbolOpt: Option[InputSymbol], stackSymbol: StackSymbol): (S, Seq[StackSymbol])
}
