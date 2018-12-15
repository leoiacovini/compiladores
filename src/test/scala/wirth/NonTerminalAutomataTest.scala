package wirth

import common.automata.ndfa.NDFARunner
import consumers.ConsumeLine
import consumers.ConsumeLine.AsciiChar
import org.scalatest.WordSpec

class NonTerminalAutomataTest extends WordSpec {
  val runner: NDFARunner[AsciiChar, NonTerminalAutomata.NonTerminalAutomataState] = NDFARunner.fromNDFA(new NonTerminalAutomata)
  "A NonTerminal automata" must {
    "accept letter {letter | digit}" in {
      val nonterminal = Seq(
        AsciiChar(c = 'A', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
        AsciiChar(c = 'B', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
        AsciiChar(c = '0', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit),
        AsciiChar(c = 'C', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
        AsciiChar(c = '3', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit)
      )

      assert(runner.runAll(nonterminal).isAccepted)
    }

    "not accept spaces" in {
      val nonterminalspace = Seq(
        AsciiChar(c = 'A', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
        AsciiChar(c = 'B', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
        AsciiChar(c = ' ', baf = ConsumeLine.Disposable, asciiCategory = ConsumeLine.Delimiter),
        AsciiChar(c = '0', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit),
        AsciiChar(c = 'C', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
        AsciiChar(c = '3', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit)
      )

      assert(!runner.runAll(nonterminalspace).isAccepted)
    }
  }
}
