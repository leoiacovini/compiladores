package wirth

import common.automata.ndfa.NonDeterministicFiniteAutomata
import consumers.ConsumeLine
import consumers.ConsumeLine.AsciiChar
import wirth.NonTerminalAutomata.{LetterDigitRepeat, NonTerminalAutomataState, TrapState, WaitingLetter}

object NonTerminalAutomata {
  sealed trait NonTerminalAutomataState
  object WaitingLetter extends NonTerminalAutomataState
  object LetterDigitRepeat extends NonTerminalAutomataState
  object TrapState extends NonTerminalAutomataState
}
class NonTerminalAutomata extends NonDeterministicFiniteAutomata[AsciiChar, NonTerminalAutomataState] {
  override val alphabet: Seq[AsciiChar] = Seq()
  override val initialStates: Seq[NonTerminalAutomataState] = WaitingLetter :: Nil
  override val states: Seq[NonTerminalAutomataState] = Seq(WaitingLetter, LetterDigitRepeat)
  override val acceptStates: Seq[NonTerminalAutomataState] = Seq(LetterDigitRepeat)

  def transition[S >: NonTerminalAutomataState](state: S, symbol: AsciiChar): Seq[NonTerminalAutomataState] = {
    (state, symbol) match {
      case (WaitingLetter, AsciiChar(_, _, ConsumeLine.Letter)) => LetterDigitRepeat :: Nil
      case (LetterDigitRepeat, AsciiChar(_, _, ConsumeLine.Digit)) => LetterDigitRepeat :: Nil
      case (LetterDigitRepeat, AsciiChar(_, _, ConsumeLine.Letter)) => LetterDigitRepeat :: Nil
      case _ => TrapState :: Nil
    }
  }
}
