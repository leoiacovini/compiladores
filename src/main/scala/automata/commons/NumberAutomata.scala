package automata.commons

import automata.commons.NumberState.NumberState
import automata.{CharAlphabets, NonDeterministicFiniteAutomata}
object NumberState {
  sealed trait NumberState
  case object Start extends NumberState
  case object Repeat extends NumberState
  case object NotNumber extends NumberState

}

object NumberAutomata extends NonDeterministicFiniteAutomata[Char, NumberState] {
  override val alphabet: Seq[Char] = (CharAlphabets.Alphanumeric ++ CharAlphabets.Special).toSeq
  override val initialState: NumberState = NumberState.Start
  override val states: Seq[NumberState] = Seq(NumberState.Start, NumberState.Repeat, NumberState.NotNumber)
  override val acceptStates: Seq[NumberState] = Seq(NumberState.Repeat)

  override def transition[S >: NumberState](state: S, symbol: Char): Seq[NumberState] = {
    (state, symbol) match {
      case (NumberState.Start, c) if c.isDigit => Seq(NumberState.Repeat)
      case (NumberState.Repeat, c) if c.isDigit => Seq(NumberState.Repeat)
      case _ => Seq(NumberState.NotNumber)
    }
  }
}
