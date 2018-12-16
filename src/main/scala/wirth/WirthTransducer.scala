package wirth

import common.automata.ndfa.NDFARunner
import common.automata.{LexicalToken, Transducer}
import consumers.ConsumeLine
import consumers.ConsumeLine.AsciiChar

class WirthTransducer extends Transducer {
  val terminalRunner: NDFARunner[ConsumeLine.AsciiChar, TerminalAutomata.TerminalAutomataState] = NDFARunner.fromNDFA(new TerminalAutomata)
  val nonTerminalRunner: NDFARunner[AsciiChar, NonTerminalAutomata.NonTerminalAutomataState] = NDFARunner.fromNDFA(new NonTerminalAutomata)
  val otherSymbolsRunner: NDFARunner[AsciiChar, OtherSymbolsAutomata.OtherSymbolsAutomataState] = NDFARunner.fromNDFA(new OtherSymbolsAutomata)

  val initial = Map(
    "Terminal" -> terminalRunner,
    "NonTerminal" -> nonTerminalRunner,
    "OtherSymbols" -> otherSymbolsRunner
  )
  def getNext[State](before: NDFARunner[AsciiChar, State],
                     after: NDFARunner[AsciiChar, State],
                     reset: NDFARunner[AsciiChar, State],
                     typ: String,
                     beforeRawValue: String): (NDFARunner[AsciiChar, State], Option[LexicalToken]) = {
    (before, after) match {
      case (b, a) if b.isAccepted && !a.isAccepted => (reset, Some(LexicalToken(typ, beforeRawValue)))
      case (b, a) if b.isAccepted => (a, Some(LexicalToken(typ, beforeRawValue)))
      case (_, a) => (a, None)
    }
  }

  override def transduce(chars: Seq[ConsumeLine.AsciiChar]): Seq[LexicalToken] = {
    val l : Option[LexicalToken]= None
    val usefullChars = chars.filter(_.baf == ConsumeLine.Useful)
    (0 to usefullChars.size).foldLeft((initial, l)) {case ((before, lexical), index) =>
      val nextValue = usefullChars.slice(0, index + 1)
      val beforeRawValue = new String(usefullChars.slice(0, index).filter(_.baf == ConsumeLine.Useful).map(_.c).toArray)
      val beforeTerminal = before("Terminal").asInstanceOf[NDFARunner[ConsumeLine.AsciiChar, TerminalAutomata.TerminalAutomataState]]
      val beforeNonTerminal = before("NonTerminal").asInstanceOf[NDFARunner[AsciiChar, NonTerminalAutomata.NonTerminalAutomataState]]
      val beforeOtherSymbols = before("OtherSymbols").asInstanceOf[NDFARunner[AsciiChar, OtherSymbolsAutomata.OtherSymbolsAutomataState]]
      val afterTerminalRunner = terminalRunner.runAll(nextValue)
      val afterNonTerminalRunner = nonTerminalRunner.runAll(nextValue)
      val afterOtherSymbolsRunner = otherSymbolsRunner.runAll(nextValue)

      val (nextTerminalRunner, lexicalTerminal) = getNext(beforeTerminal, afterTerminalRunner, terminalRunner, "Terminal", beforeRawValue)
      val (nextNonTerminalRunner, lexicalNonTerminal) = getNext(beforeNonTerminal, afterNonTerminalRunner, nonTerminalRunner, "NonTerminal", beforeRawValue)
      val (nextOtherSymbolsRunner, lexicalOtherSymbols) = getNext(beforeOtherSymbols, afterOtherSymbolsRunner, otherSymbolsRunner, "OtherSymbols", beforeRawValue)
      val nextState = Map(
        "Terminal" -> nextTerminalRunner,
        "NonTerminal" -> nextNonTerminalRunner,
        "OtherSymbols" -> nextOtherSymbolsRunner
      )
      val nextLexicalToken = (lexical :: lexicalTerminal :: lexicalNonTerminal :: lexicalOtherSymbols :: Nil).collectFirst { case Some(lt) => lt }

      (nextState, nextLexicalToken)
    } match {
      case (_, Some(lexicalToken)) =>
        Seq(lexicalToken) ++ transduce(usefullChars.drop(lexicalToken.rawValue.length))
      case (_, None) if usefullChars.isEmpty => Seq.empty
      case (_, None) => throw new Exception(s"Could not find any token in $chars")
    }
  }

  def transform(lex: Seq[LexicalToken]): Seq[WirthLexicalToken] = {
    lex.map {
      case LexicalToken("Terminal", rawValue) => Terminal(rawValue.drop(1).dropRight(1))
      case LexicalToken("NonTerminal", rawValue) => NonTerminal(rawValue)
      case LexicalToken("OtherSymbols", "{") => Special("{")
      case LexicalToken("OtherSymbols", "[") => Special("[")
      case LexicalToken("OtherSymbols", "(") => Special("(")
      case LexicalToken("OtherSymbols", ")") => Special(")")
      case LexicalToken("OtherSymbols", "]") => Special("]")
      case LexicalToken("OtherSymbols", "}") => Special("}")
      case LexicalToken("OtherSymbols", "|") => Special("|")

      case LexicalToken("OtherSymbols", ".") => Special(".")
      case LexicalToken("OtherSymbols", "->") => Arrow
    }
  }
}
