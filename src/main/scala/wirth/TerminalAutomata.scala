package wirth

import common.automata.ndfa.NonDeterministicFiniteAutomata
import consumers.ConsumeLine.AsciiChar
import wirth.TerminalAutomata._

object TerminalAutomata {
  sealed trait TerminalAutomataState
  object WaitingQuote extends TerminalAutomataState
  object AnyCharacterRepeat extends TerminalAutomataState
  object AcceptTerminal extends TerminalAutomataState
  object EscapeMode extends TerminalAutomataState
  object TrapState extends TerminalAutomataState
}

class TerminalAutomata extends NonDeterministicFiniteAutomata[AsciiChar, TerminalAutomataState] {
  override val alphabet: Seq[AsciiChar] = Seq()
  override val initialStates: Seq[TerminalAutomataState] = WaitingQuote :: Nil
  override val states: Seq[TerminalAutomataState] = Seq(WaitingQuote, AnyCharacterRepeat, AcceptTerminal)
  override val acceptStates: Seq[TerminalAutomataState] = Seq(AcceptTerminal)

  def transition[S >: TerminalAutomataState](state: S, symbol: AsciiChar): Seq[TerminalAutomataState] = {
    (state, symbol) match {
      case (WaitingQuote, AsciiChar('"', _, _)) => AnyCharacterRepeat :: Nil
      case (AnyCharacterRepeat, AsciiChar(c, _, _)) if c != '\\' && c != '"' => AnyCharacterRepeat :: Nil
      case (AnyCharacterRepeat, AsciiChar(c, _, _)) if c == '\\' => EscapeMode :: Nil
      case (EscapeMode, AsciiChar(c, _, _)) if Seq('\\', '"').contains(c) => AnyCharacterRepeat :: Nil
      case (AnyCharacterRepeat, AsciiChar(c, _, _)) if c == '"' => AcceptTerminal :: Nil
      case _ => TrapState :: Nil
    }
  }
}
