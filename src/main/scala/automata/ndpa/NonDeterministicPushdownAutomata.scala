package automata.ndpa

import wirth.WirthToNDPA.StackAlphabet

trait NonDeterministicPushdownAutomata[InputSymbol, StackSymbol, +State] {
  val id: String = "common-id"
  val inputAlphabet: Seq[InputSymbol]
  val stackAlphabet: Seq[StackSymbol]
  val initialStackSymbol: StackSymbol
  val initialState: State
  val states: Seq[State]
  val acceptStates: Seq[State]
  val trapState: State
//  def accepts[S >: State](state: S): Boolean = acceptStates.contains(state)
  def transition[S >: State](state: S, inputSymbolOpt: Option[InputSymbol], stackSymbolOpt: Option[StackSymbol]): Seq[(S, Seq[StackSymbol])]
  def copy[S >: State](newInputAlphabet: Seq[InputSymbol] = inputAlphabet,
                       newStackAlphabet: Seq[StackSymbol] = stackAlphabet,
                       newInitialStackSymbol: StackSymbol = initialStackSymbol,
                       newInitialState: S = initialState,
                       newStates: Seq[S] = states,
                       newAcceptStates: Seq[S] = acceptStates,
                       newTrapState: S = trapState,
                       newId: String = id,
                       newTransition: (S, Option[InputSymbol], Option[StackSymbol]) => Seq[(S, Seq[StackSymbol])] = this.transition _) = new NonDeterministicPushdownAutomata[InputSymbol, StackSymbol, S] {
    override val inputAlphabet: Seq[InputSymbol] = newInputAlphabet
    override val stackAlphabet: Seq[StackSymbol] = newStackAlphabet
    override val initialStackSymbol: StackSymbol = newInitialStackSymbol
    override val initialState: S = newInitialState
    override val states: Seq[S] = newStates
    override val acceptStates: Seq[S] = newAcceptStates
    override val trapState: S = newTrapState

    override def transition[St >: S](state: St, inputSymbolOpt: Option[InputSymbol], stackSymbolOpt: Option[StackSymbol]): Seq[(St, Seq[StackSymbol])] = newTransition(state.asInstanceOf[S], inputSymbolOpt, stackSymbolOpt)
  }

  def toPrettyString: String = {
    s"""
      |NDPA: ${super.toString}
      |initialState: $initialState
    """.stripMargin
  }
}

object NonDeterministicPushdownAutomata {
  def printTransitions[InputSymbol, StackSymbol, State](ndpa: NonDeterministicPushdownAutomata[InputSymbol, StackSymbol, State],
                                                        tries: Seq[(State, Option[InputSymbol], Option[StackSymbol])]): Unit = {
    println(
      tries
        .map { case (st, inputOpt, stackOpt) =>

          val tuples = ndpa.transition(st, inputOpt, stackOpt)
          if(tuples.nonEmpty)
            s"$st, $inputOpt, $stackOpt -> $tuples"
          else ""
        }.filterNot(_.isEmpty)
        .mkString("\n")
    )
  }
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
    println(ndpa1.states)
    println(ndpa2.states)
//    require(isDisjoint(ndpa1.states, ndpa2.states), "States must be disjoint")

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
          case _ if !(ndpa1.states ++ ndpa2.states).contains(state) =>
//            println(s"WARN State $state is not found in any of the or candidates")
            Seq.empty
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

  implicit class NDPABuilder[InputSymbol, StackSymbol, State](ndpa: NonDeterministicPushdownAutomata[InputSymbol, StackSymbol, State]) {
    def addTransition[S >: State](state: S, inputSymbolOpt: Option[InputSymbol], stackSymbolOpt: Option[StackSymbol], result: Seq[(State, Seq[StackSymbol])]): NonDeterministicPushdownAutomata[InputSymbol, StackSymbol, State] = {
      ndpa.copy(
        newTransition = (nstate, ninputSymbolOpt, nstackSymbolOpt) => {
          val added = if(state == nstate && ninputSymbolOpt == inputSymbolOpt && nstackSymbolOpt == stackSymbolOpt) result else Seq.empty
          ndpa.transition(nstate, ninputSymbolOpt, nstackSymbolOpt) ++ added
        }
      )
    }
    def addTransition[S >: State](states: Seq[S], inputSymbolOpt: Option[InputSymbol], stackSymbolOpt: Option[StackSymbol], result: Seq[(State, Seq[StackSymbol])]): NonDeterministicPushdownAutomata[InputSymbol, StackSymbol, State] = {
      states.foldLeft(ndpa) {(ndpa, state) => ndpa.addTransition(state, inputSymbolOpt, stackSymbolOpt, result)}
    }
    def replaceInitialState(initialState: State): NonDeterministicPushdownAutomata[InputSymbol, StackSymbol, State] = ndpa.copy(newInitialState = initialState)
    def addState(state: State): NonDeterministicPushdownAutomata[InputSymbol, StackSymbol, State] = ndpa.copy(newStates = (ndpa.states :+ state).distinct)
    def replaceAcceptStates(newAcceptStates: Seq[State]): NonDeterministicPushdownAutomata[InputSymbol, StackSymbol, State] = ndpa.copy(newAcceptStates = newAcceptStates)
  }
}
