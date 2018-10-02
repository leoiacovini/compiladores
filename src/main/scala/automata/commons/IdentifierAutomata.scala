package automata.commons

import automata.{CharAlphabets, NonDeterministicFiniteAutomata}

object IdentifierAutomata {
  sealed trait IdentifierState
  case object Start extends IdentifierState
  case object Repeat extends IdentifierState
  case object NotIdentifier extends IdentifierState
  val IdentifierAutomata: NonDeterministicFiniteAutomata[Char, IdentifierState] = new NonDeterministicFiniteAutomata[Char, IdentifierState] {
    override val alphabet: Set[Char] = CharAlphabets.Alphanumeric
    override val initialState: IdentifierState = Start
    override val states: Set[IdentifierState] = Set(Start, Repeat, NotIdentifier)
    override val acceptStates: Set[IdentifierState] = Set(Repeat)

    override def transition(state: IdentifierState, symbol: Char): Set[IdentifierState] = {
      (state, symbol) match {
        case (Start, c) if c.isLetter => Set(Repeat)
        case (Repeat, c) if c.isLetterOrDigit => Set(Repeat)
        case _ => Set(NotIdentifier)
      }
    }
  }
}
