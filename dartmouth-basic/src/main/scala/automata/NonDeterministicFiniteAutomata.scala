package automata

trait NonDeterministicFiniteAutomata[Symbol, State] {
  val alphabet: Set[Symbol]
  val initialState: State
  val states: Set[State]
  val acceptStates: Set[State]
  def transition(state: State, symbol: Symbol): Set[State]
}

object NonDeterministicFiniteAutomata {
  def accepts[Symbol, State](nonDeterministicFiniteAutomata: NonDeterministicFiniteAutomata[Symbol, State], symbols: Seq[Symbol]): Boolean = {
    symbols.foldLeft(Set(nonDeterministicFiniteAutomata.initialState)) { (states, symbol) =>
      for {
        state <- states
        newStates <- nonDeterministicFiniteAutomata.transition(state, symbol)
      } yield newStates
    }.exists(nonDeterministicFiniteAutomata.acceptStates)
  }

  def concat[Symbol, State](ndfa1: NonDeterministicFiniteAutomata[Symbol, State],
                            ndfa2: NonDeterministicFiniteAutomata[Symbol, State]): NonDeterministicFiniteAutomata[Symbol, State] = {
    require(ndfa1.states.intersect(ndfa2.states).isEmpty, "States must be disjoint")
    new NonDeterministicFiniteAutomata[Symbol, State] {
      override val alphabet: Set[Symbol] = ndfa1.alphabet ++ ndfa2.alphabet
      override val initialState: State = ndfa1.initialState
      override val states: Set[State] = ndfa1.states ++ ndfa1.states
      override val acceptStates: Set[State] = ndfa2.acceptStates

      override def transition(state: State, symbol: Symbol): Set[State] = {
        if(ndfa1.states.contains(state)) {
          val newStates = ndfa1.transition(state, symbol)
          if (newStates.exists(ndfa1.acceptStates))
            newStates + ndfa2.initialState
          else
            newStates
        } else {
          ndfa2.transition(state, symbol)
        }
      }
    }
  }
}
