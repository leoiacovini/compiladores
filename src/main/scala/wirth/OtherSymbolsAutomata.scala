package wirth

import common.automata.ndfa.NonDeterministicFiniteAutomata
import consumers.ConsumeLine.AsciiChar
import wirth.OtherSymbolsAutomata._
object OtherSymbolsAutomata {
  sealed trait OtherSymbolsAutomataState
  object WaitingSymbols extends OtherSymbolsAutomataState
  object OpenBracket extends OtherSymbolsAutomataState
  object OpenParentesis extends OtherSymbolsAutomataState
  object OpenCurlyBracket extends OtherSymbolsAutomataState
  object CloseBracket extends OtherSymbolsAutomataState
  object CloseParentesis extends OtherSymbolsAutomataState
  object CloseCurlyBracket extends OtherSymbolsAutomataState
  object Or extends OtherSymbolsAutomataState
  object RightArrowHead extends OtherSymbolsAutomataState
  object RightArrowBody extends OtherSymbolsAutomataState
  object Dot extends OtherSymbolsAutomataState
  object TrapState extends OtherSymbolsAutomataState

}

class OtherSymbolsAutomata extends NonDeterministicFiniteAutomata[AsciiChar, OtherSymbolsAutomataState] {
  import wirth.OtherSymbolsAutomata._
  override val alphabet: Seq[AsciiChar] = Seq()
  override val initialStates: Seq[OtherSymbolsAutomataState] = WaitingSymbols :: Nil
  override val states: Seq[OtherSymbolsAutomataState] =
    Seq(WaitingSymbols,
      OpenBracket,
      OpenCurlyBracket,
      OpenParentesis,
      CloseBracket,
      CloseCurlyBracket,
      CloseParentesis,
      Or,
      RightArrowBody,
      RightArrowHead,
      Dot,
      TrapState)
  override val acceptStates: Seq[OtherSymbolsAutomataState] = Seq(
    OpenBracket,
    OpenCurlyBracket,
    OpenParentesis,
    CloseBracket,
    CloseCurlyBracket,
    CloseParentesis,
    Or,
    RightArrowHead,
    Dot)

  def transition[S >: OtherSymbolsAutomataState](state: S, symbol: AsciiChar): Seq[OtherSymbolsAutomataState] = {
    (state, symbol) match {
      case (WaitingSymbols, AsciiChar('[', _, _)) => OpenBracket :: Nil
      case (WaitingSymbols, AsciiChar('(', _, _)) => OpenParentesis :: Nil
      case (WaitingSymbols, AsciiChar('{', _, _)) => OpenCurlyBracket :: Nil
      case (WaitingSymbols, AsciiChar(']', _, _)) => CloseBracket :: Nil
      case (WaitingSymbols, AsciiChar(')', _, _)) => CloseParentesis :: Nil
      case (WaitingSymbols, AsciiChar('}', _, _)) => CloseCurlyBracket :: Nil
      case (WaitingSymbols, AsciiChar('.', _, _)) => Dot :: Nil
      case (WaitingSymbols, AsciiChar('|', _, _)) => Or :: Nil
      case (WaitingSymbols, AsciiChar('-', _, _)) => RightArrowBody :: Nil
      case (RightArrowBody, AsciiChar('>', _, _)) => RightArrowHead :: Nil
      case _ => TrapState :: Nil
    }
  }
}
