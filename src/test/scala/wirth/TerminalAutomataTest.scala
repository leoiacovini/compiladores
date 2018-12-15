package wirth

import common.automata.ndfa.NDFARunner
import common.automata.ndpa.{NDPARunner, RunHistoryItem}
import basic.{BasicToLLVM, Print}
import consumers.ConsumeLine
import consumers.ConsumeLine.AsciiChar
import llvm.{LLVMProgram, OutputWriter}
import org.scalatest.WordSpec

class TerminalAutomataTest extends WordSpec {
  val runner: NDFARunner[AsciiChar, TerminalAutomata.TerminalAutomataState] = NDFARunner.fromNDFA(new TerminalAutomata)
  "A Terminal common.automata" must {
    "accept quoted strings" in {
      val quoted = Seq(
        AsciiChar(c = '"', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special),
        AsciiChar(c = 'L', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
        AsciiChar(c = 'E', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
        AsciiChar(c = 'T', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
        AsciiChar(c = '"', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special)
      )

      assert(runner.runAll(quoted).isAccepted)
    }

    "accept escaped quote" in {
      val escapequoted = Seq(
        AsciiChar(c = '"', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special),
        AsciiChar(c = 'L', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
        AsciiChar(c = '\\', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special),
        AsciiChar(c = '"', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special),
        AsciiChar(c = 'E', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
        AsciiChar(c = 'T', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
        AsciiChar(c = '"', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special)
      )

      assert(runner.runAll(escapequoted).isAccepted)
    }
  }
}
