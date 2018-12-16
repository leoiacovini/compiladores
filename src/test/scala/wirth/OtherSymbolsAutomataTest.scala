package wirth

import common.automata.ndfa.NDFARunner
import consumers.ConsumeLine
import consumers.ConsumeLine.AsciiChar
import org.scalatest.WordSpec

class OtherSymbolsAutomataTest extends WordSpec {
  val runner: NDFARunner[AsciiChar, OtherSymbolsAutomata.OtherSymbolsAutomataState] = NDFARunner.fromNDFA(new OtherSymbolsAutomata)
  "A OtherSymbols Automata" must {
    "accept (" in {
      val symbol = Seq(
        AsciiChar(c = '(', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special)
      )

      assert(runner.runAll(symbol).isAccepted)
    }

    "accept {" in {
      val symbol = Seq(
        AsciiChar(c = '{', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special)
      )

      assert(runner.runAll(symbol).isAccepted)
    }

    "accept [" in {
      val symbol = Seq(
        AsciiChar(c = '[', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special)
      )

      assert(runner.runAll(symbol).isAccepted)
    }

    "accept )" in {
      val symbol = Seq(
        AsciiChar(c = ')', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special)
      )

      assert(runner.runAll(symbol).isAccepted)
    }

    "accept }" in {
      val symbol = Seq(
        AsciiChar(c = '}', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special)
      )

      assert(runner.runAll(symbol).isAccepted)
    }

    "accept ]" in {
      val symbol = Seq(
        AsciiChar(c = ']', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special)
      )

      assert(runner.runAll(symbol).isAccepted)
    }

    "accept ." in {
      val symbol = Seq(
        AsciiChar(c = '.', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special)
      )

      assert(runner.runAll(symbol).isAccepted)
    }

    "accept ->" in {
      val symbol = Seq(
        AsciiChar(c = '-', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special),
        AsciiChar(c = '>', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special)
      )

      assert(runner.runAll(symbol).isAccepted)
    }

    "reject ->\"" in {
      val symbol =
        Seq(
          AsciiChar(c = '-', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special),
          AsciiChar(c = '>', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special),
          AsciiChar(c = '3', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit)
        )


      assert(!runner.runAll(symbol).isAccepted)
    }
  }
}
