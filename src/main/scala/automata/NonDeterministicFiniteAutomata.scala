package automata

trait NonDeterministicFiniteAutomata[Symbol, +State] {
  val alphabet: Seq[Symbol]
  val initialState: State
  val states: Seq[State]
  val acceptStates: Seq[State]
  def transition[S >: State](state: S, symbol: Symbol): Seq[State]
}

object NonDeterministicFiniteAutomata {
  def accepts[Symbol, State](nonDeterministicFiniteAutomata: NonDeterministicFiniteAutomata[Symbol, State], symbols: Seq[Symbol]): Boolean = {
    symbols.foldLeft(Set(nonDeterministicFiniteAutomata.initialState)) { (states, symbol) =>
      for {
        state <- states
        newStates <- nonDeterministicFiniteAutomata.transition(state, symbol)
      } yield newStates
    }.exists(nonDeterministicFiniteAutomata.acceptStates.toSet)
  }

  def isDisjoint[Symbol, State](ndfas: NonDeterministicFiniteAutomata[Symbol, State]*): Boolean = {
    (for {
      ndfa1 <- ndfas
      ndfa2 <- ndfas
    } yield ndfa1.states.intersect(ndfa2.states).isEmpty) forall(_ == true)
  }

  def concat[Symbol, State](ndfa1: NonDeterministicFiniteAutomata[Symbol, State],
                            ndfa2: NonDeterministicFiniteAutomata[Symbol, State]): NonDeterministicFiniteAutomata[Symbol, State] = {
    require(ndfa1.states.intersect(ndfa2.states).isEmpty, "States must be disjoint")
    new NonDeterministicFiniteAutomata[Symbol, State] {
      override val alphabet: Seq[Symbol] = ndfa1.alphabet ++ ndfa2.alphabet
      override val initialState: State = ndfa1.initialState
      override val states: Seq[State] = ndfa1.states ++ ndfa1.states
      override val acceptStates: Seq[State] = ndfa2.acceptStates

      override def transition[S >: State](state: S, symbol: Symbol): Seq[State] = {
        if(ndfa1.states.contains(state)) {
          val newStates = ndfa1.transition(state, symbol)
          if (newStates.exists(ndfa1.acceptStates.contains))
            newStates :+ ndfa2.initialState
          else
            newStates
        } else {
          ndfa2.transition(state, symbol)
        }
      }
    }
  }
}
