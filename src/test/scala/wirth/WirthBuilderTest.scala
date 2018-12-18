package wirth

import common.automata.LexicalToken
import consumers.ConsumeLine
import consumers.ConsumeLine.AsciiChar
import org.scalatest.WordSpec

class WirthBuilderTest extends WordSpec {

  "WirthBuilder" must {
    "build a simple rule grammar" in {
      val lex = Seq(
        NonTerminal("A"),
        Arrow,
        Terminal("3"),
        Special(".")
      )

      assert(WirthGrammarParser.parseGrammar(lex) == Map(NonTerminalToken("A") -> TerminalToken("3")))
    }

    "build a 2 rules grammar" in {
      val lex = Seq(
        NonTerminal("A"),
        Arrow,
        Terminal("3"),
        Special("."),
        NonTerminal("B"),
        Arrow,
        NonTerminal("A"),
        Special("|"),
        NonTerminal("B"),
        Special(".")
      )

      assert(WirthGrammarParser.parseGrammar(lex) == Map(
        NonTerminalToken("A") -> TerminalToken("3"),
        NonTerminalToken("B") -> Or(NonTerminalToken("A"), NonTerminalToken("B"))
      ))
    }
  }
}
