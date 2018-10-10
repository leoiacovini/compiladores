package automata.commons

import automata.commons.SpecialState.SpecialState
import automata.CharAlphabets
import automata.ndfa.NonDeterministicFiniteAutomata

object SpecialState {

  sealed trait SpecialState
  case object Start extends SpecialState
  case class MaybeDone(c: Char) extends SpecialState
  case object Done extends SpecialState
  case object NotSpecial extends SpecialState

}

object SpecialAutomata extends NonDeterministicFiniteAutomata[Char, SpecialState] {

  override val alphabet: Seq[Char] = (CharAlphabets.Special ++ CharAlphabets.Alphanumeric).toSeq
  override val initialStates: Seq[SpecialState] = Seq(SpecialState.Start)
  override val states: Seq[SpecialState] = Seq(SpecialState.Start, SpecialState.MaybeDone('>'), SpecialState.MaybeDone('<'), SpecialState.Done, SpecialState.NotSpecial)
  override val acceptStates: Seq[SpecialState] = Seq(SpecialState.Done, SpecialState.MaybeDone('>'), SpecialState.MaybeDone('<'))

  override def transition[S >: SpecialState](state: S, symbol: Char): Seq[SpecialState] = {
    (state, symbol) match {
      case (SpecialState.Start, c) if c == '>' || c == '<' => Seq(SpecialState.MaybeDone(c))
      case (SpecialState.Start, c) if CharAlphabets.isSpecial(c) => Seq(SpecialState.Done)
      case (SpecialState.MaybeDone(_), '=') => Seq(SpecialState.Done)
      case (SpecialState.MaybeDone('<'), '>') => Seq(SpecialState.Done)
      case (SpecialState.Done, _) => Seq(SpecialState.NotSpecial)
      case _ => Seq(SpecialState.NotSpecial)
    }
  }

}
