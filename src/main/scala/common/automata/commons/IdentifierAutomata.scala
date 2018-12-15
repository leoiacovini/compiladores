package common.automata.commons

import common.automata.commons.IdentifierState.{IdentifierState, NotIdentifier}
import common.automata.CharAlphabets
import common.automata.ndfa.NonDeterministicFiniteAutomata
object IdentifierState {
  sealed trait IdentifierState
  case object Start extends IdentifierState
  case object Repeat extends IdentifierState
  case object NotIdentifier extends IdentifierState

}

object IdentifierAutomata extends NonDeterministicFiniteAutomata[Char, IdentifierState] {
  override val alphabet: Seq[Char] = CharAlphabets.Alphanumeric.toSeq
  override val initialStates: Seq[IdentifierState] = Seq(IdentifierState.Start)
  override val states: Seq[IdentifierState] = Seq(IdentifierState.Start, IdentifierState.Repeat, IdentifierState.NotIdentifier)
  override val acceptStates: Seq[IdentifierState] = Seq(IdentifierState.Repeat)

  override def transition[S >: IdentifierState](state: S, symbol: Char): Seq[IdentifierState] = {
    (state, symbol) match {
      case (IdentifierState.Start, c) if c.isLetter => Seq(IdentifierState.Repeat)
      case (IdentifierState.Repeat, c) if c.isLetterOrDigit => Seq(IdentifierState.Repeat)
      case _ => Seq(NotIdentifier)
    }
  }
}
