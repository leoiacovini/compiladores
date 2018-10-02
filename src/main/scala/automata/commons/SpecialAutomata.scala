package automata.commons

import automata.{CharAlphabets, NonDeterministicFiniteAutomata}

object SpecialAutomata {
  sealed trait SpecialState
  case object Start extends SpecialState
  case class MaybeDone(c: Char) extends SpecialState
  case object Done extends SpecialState
  case object NotSpecial extends SpecialState
  val SpecialAutomata: NonDeterministicFiniteAutomata[Char, SpecialState] = new NonDeterministicFiniteAutomata[Char, SpecialState] {
    override val alphabet: Set[Char] = CharAlphabets.Special ++ CharAlphabets.Alphanumeric
    override val initialState: SpecialState = Start
    override val states: Set[SpecialState] = Set(Start, MaybeDone('>'), MaybeDone('<'), Done, NotSpecial)
    override val acceptStates: Set[SpecialState] = Set(Done, MaybeDone('>'), MaybeDone('<'))

    override def transition(state: SpecialState, symbol: Char): Set[SpecialState] = {
      (state, symbol) match {
        case (Start, c) if c == '>' || c == '<' => Set(MaybeDone(c))
        case (Start, c) if CharAlphabets.isSpecial(c) => Set(Done)
        case (MaybeDone(_), '=') => Set(Done)
        case (MaybeDone('<'), '>') => Set(Done)
        case (Done, _) => Set(NotSpecial)
        case _ => Set(NotSpecial)
      }
    }
  }

}
