package automata.ndpa

trait NonDeterministicPushdownAutomata[InputSymbol, StackSymbol, +State] {
  val inputAlphabet: Seq[InputSymbol]
  val stackAlphabet: Seq[StackSymbol]
  val initialStackSymbol: StackSymbol
  val initialState: State
  val states: Seq[State]
  val acceptStates: Seq[State]
  val trapState: State
//  def accepts[S >: State](state: S): Boolean = acceptStates.contains(state)
  def transition[S >: State](state: S, inputSymbolOpt: Option[InputSymbol], stackSymbolOpt: Option[StackSymbol]): Seq[(S, Seq[StackSymbol])]
}

object NonDeterministicPushdownAutomata {
  def isDisjoint[A](x: Seq[A], y: Seq[A]): Boolean = x.intersect(y).isEmpty && y.intersect(x).isEmpty
  def concat[InputSymbol, StackSymbol, State](ndpa1: NonDeterministicPushdownAutomata[InputSymbol, StackSymbol, State],
                                              ndpa2: NonDeterministicPushdownAutomata[InputSymbol, StackSymbol, State]): NonDeterministicPushdownAutomata[InputSymbol, StackSymbol, State] = {
    require(isDisjoint(ndpa1.states, ndpa2.states), "States must be disjoint")

    new NonDeterministicPushdownAutomata[InputSymbol, StackSymbol, State] {
      override val inputAlphabet: Seq[InputSymbol] = (ndpa1.inputAlphabet ++ ndpa2.inputAlphabet).distinct
      override val stackAlphabet: Seq[StackSymbol] = (ndpa1.stackAlphabet ++ ndpa2.stackAlphabet).distinct
      override val initialStackSymbol: StackSymbol = ndpa1.initialStackSymbol
      override val initialState: State= ndpa1.initialState
      override val states: Seq[State] = ndpa1.states ++ ndpa2.states
      override val acceptStates: Seq[State] = ndpa2.acceptStates
      override val trapState: State = ndpa1.trapState

      override def transition[S >: State](state: S, inputSymbolOpt: Option[InputSymbol], stackSymbolOpt: Option[StackSymbol]): Seq[(S, Seq[StackSymbol])] = {
        (inputSymbolOpt, stackSymbolOpt) match {
          case (None, None) if ndpa1.acceptStates.contains(state) => ndpa1.transition(state, None, None) :+ (ndpa2.initialState, Seq(ndpa2.initialStackSymbol))
          case _ if ndpa1.states.contains(state) => ndpa1.transition(state, inputSymbolOpt, stackSymbolOpt)
          case _ if ndpa2.states.contains(state) => ndpa2.transition(state, inputSymbolOpt, stackSymbolOpt)
        }
      }
    }
  }

  def or[InputSymbol, StackSymbol, State](ndpa1: NonDeterministicPushdownAutomata[InputSymbol, StackSymbol, State],
                                          ndpa2: NonDeterministicPushdownAutomata[InputSymbol, StackSymbol, State]): NonDeterministicPushdownAutomata[InputSymbol, StackSymbol, State] = {
    require(isDisjoint(ndpa1.states, ndpa2.states), "States must be disjoint")

    new NonDeterministicPushdownAutomata[InputSymbol, StackSymbol, State] {
      override val inputAlphabet: Seq[InputSymbol] = (ndpa1.inputAlphabet ++ ndpa2.inputAlphabet).distinct
      override val stackAlphabet: Seq[StackSymbol] = (ndpa1.stackAlphabet ++ ndpa2.stackAlphabet).distinct
      override val initialStackSymbol: StackSymbol = ndpa1.initialStackSymbol
      override val initialState: State= ndpa1.initialState
      override val states: Seq[State] = ndpa1.states ++ ndpa2.states
      override val acceptStates: Seq[State] = ndpa1.acceptStates ++ ndpa2.acceptStates
      override val trapState: State = ndpa1.trapState

      override def transition[S >: State](state: S, inputSymbolOpt: Option[InputSymbol], stackSymbolOpt: Option[StackSymbol]): Seq[(S, Seq[StackSymbol])] = {
        (inputSymbolOpt, stackSymbolOpt) match {
          case (None, Some(ndpa1.initialStackSymbol)) if ndpa1.initialState == state =>
            Seq((ndpa2.initialState, Seq(ndpa2.initialStackSymbol)))
          case _ if ndpa1.states.contains(state) => ndpa1.transition(state, inputSymbolOpt, stackSymbolOpt)
          case _ if ndpa2.states.contains(state) => ndpa2.transition(state, inputSymbolOpt, stackSymbolOpt)
        }
      }
    }
  }

  def kleene[InputSymbol, StackSymbol, State](ndpa: NonDeterministicPushdownAutomata[InputSymbol, StackSymbol, State]): NonDeterministicPushdownAutomata[InputSymbol, StackSymbol, State] = {
    new NonDeterministicPushdownAutomata[InputSymbol, StackSymbol, State] {
      override val inputAlphabet: Seq[InputSymbol] = ndpa.inputAlphabet
      override val stackAlphabet: Seq[StackSymbol] = ndpa.stackAlphabet
      override val initialStackSymbol: StackSymbol = ndpa.initialStackSymbol
      override val initialState: State= ndpa.initialState
      override val states: Seq[State] = ndpa.states
      override val acceptStates: Seq[State] = ndpa.acceptStates
      override val trapState: State = ndpa.trapState

      override def transition[S >: State](state: S, inputSymbolOpt: Option[InputSymbol], stackSymbolOpt: Option[StackSymbol]): Seq[(S, Seq[StackSymbol])] = {
        (state, inputSymbolOpt, stackSymbolOpt) match {
          case (ndpa.initialState, None, Some(ndpa.initialStackSymbol)) => Seq((ndpa.acceptStates.head, Seq.empty))
          case (s, None, None) if ndpa.acceptStates.contains(s) =>
            Seq((ndpa.initialState, Seq(ndpa.initialStackSymbol)))
          case _ => ndpa.transition(state, inputSymbolOpt, stackSymbolOpt)
        }
      }
    }
  }
}
