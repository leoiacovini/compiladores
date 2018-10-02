package automata.commons

import automata.{CharAlphabets, NonDeterministicFiniteAutomata}

object NumberAutomata {
  sealed trait NumberState
  case object Start extends NumberState
  case object Repeat extends NumberState
  case object NotNumber extends NumberState
  val NumberAutomata: NonDeterministicFiniteAutomata[Char, NumberState] = new NonDeterministicFiniteAutomata[Char, NumberState] {
    override val alphabet: Set[Char] = CharAlphabets.Alphanumeric ++ CharAlphabets.Special
    override val initialState: NumberState = Start
    override val states: Set[NumberState] = Set(Start, Repeat, NotNumber)
    override val acceptStates: Set[NumberState] = Set(Repeat)

    override def transition(state: NumberState, symbol: Char): Set[NumberState] = {
      (state, symbol) match {
        case (Start, c) if c.isDigit => Set(Repeat)
        case (Repeat, c) if c.isDigit => Set(Repeat)
        case _ => Set(NotNumber)
      }
    }
  }
}
