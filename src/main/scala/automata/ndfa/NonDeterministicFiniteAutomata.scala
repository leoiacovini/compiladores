package automata.ndfa

trait NonDeterministicFiniteAutomata[Symbol, +State] {
  val alphabet: Seq[Symbol]
  val initialStates: Seq[State]
  val states: Seq[State]
  val acceptStates: Seq[State]
  def accepts[S >: State](states: Seq[S]): Boolean = states.exists(acceptStates.contains)
  def transition[S >: State](state: S, symbol: Symbol): Seq[State]
}

object NonDeterministicFiniteAutomata {

  def fromSeq[Symbol](symbols: Seq[Symbol]): NonDeterministicFiniteAutomata[Symbol, Int] = {
    require(symbols.nonEmpty, "Cant create a ndfa for a empty seq")
    new NonDeterministicFiniteAutomata[Symbol, Int] {
      override val alphabet: Seq[Symbol] = symbols.distinct
      override val initialStates: Seq[Int] = Seq(0)
      override val states: Seq[Int] = 0 to symbols.length
      override val acceptStates: Seq[Int] = Seq(symbols.length)

      override def transition[S >: Int](state: S, symbol: Symbol): Seq[Int] = {
        state match {
          case sta: Int if symbols.lift(sta).contains(symbol) => Seq(sta+1)
          case _ => Seq.empty
        }
      }
    }
  }

  def accepts[Symbol, State](nonDeterministicFiniteAutomata: NonDeterministicFiniteAutomata[Symbol, State], symbols: Seq[Symbol]): Boolean = {
    symbols.foldLeft(nonDeterministicFiniteAutomata.initialStates) { (states, symbol) =>
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
      override val initialStates: Seq[State] = ndfa1.initialStates
      override val states: Seq[State] = ndfa1.states ++ ndfa1.states
      override val acceptStates: Seq[State] = ndfa2.acceptStates

      override def transition[S >: State](state: S, symbol: Symbol): Seq[State] = {
        if(ndfa1.states.contains(state)) {
          val newStates = ndfa1.transition(state, symbol)
          if (newStates.exists(ndfa1.acceptStates.contains))
            newStates ++ ndfa2.initialStates
          else
            newStates
        } else {
          ndfa2.transition(state, symbol)
        }
      }
    }
  }

  def or[Symbol, State](ndfa1: NonDeterministicFiniteAutomata[Symbol, State],
                        ndfa2: NonDeterministicFiniteAutomata[Symbol, State]): NonDeterministicFiniteAutomata[Symbol, State] = {
    new NonDeterministicFiniteAutomata[Symbol, State] {
      override val alphabet: Seq[Symbol] = ndfa1.alphabet ++ ndfa2.alphabet
      override val initialStates: Seq[State] = ndfa1.initialStates ++ ndfa2.initialStates
      override val states: Seq[State] = ndfa1.states ++ ndfa1.states
      override val acceptStates: Seq[State] = ndfa1.acceptStates ++ ndfa2.acceptStates

      override def transition[S >: State](state: S, symbol: Symbol): Seq[State] = {
        val newStatesNdfa1 = ndfa1.transition(state, symbol)
        val newStatesNdfa2 = ndfa2.transition(state, symbol)
        (newStatesNdfa1 ++ newStatesNdfa2).distinct
      }
    }
  }

  def repeat[Symbol, State](ndfa: NonDeterministicFiniteAutomata[Symbol, State]): NonDeterministicFiniteAutomata[Symbol, State] = {
    new NonDeterministicFiniteAutomata[Symbol, State] {
      override val alphabet: Seq[Symbol] = ndfa.alphabet
      override val initialStates: Seq[State] = ndfa.initialStates ++ ndfa.acceptStates
      override val states: Seq[State] = ndfa.states
      override val acceptStates: Seq[State] = ndfa.acceptStates

      override def transition[S >: State](state: S, symbol: Symbol): Seq[State] = {
        val newStatesNdfa = ndfa.transition(state, symbol)
        if(ndfa.accepts(newStatesNdfa)) {
          (newStatesNdfa ++ ndfa.initialStates).distinct
        } else newStatesNdfa
      }
    }
  }
}
