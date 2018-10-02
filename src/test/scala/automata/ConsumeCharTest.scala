package automata

import automata.commons.IdentifierAutomata.IdentifierState
import consumers.ConsumeChars.ConsumeCharState
import consumers.{ConsumeChars, ConsumeLine}
import consumers.ConsumeLine.AsciiChar
import org.scalatest.WordSpec

class ConsumeCharTest extends WordSpec {

  "foo" in {
    val chars = Seq(
      AsciiChar(c = '1', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit),
      AsciiChar(c = '0', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit),
      AsciiChar(c = ' ', baf = ConsumeLine.Disposable, asciiCategory = ConsumeLine.Delimiter),
      AsciiChar(c = 'L', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
      AsciiChar(c = 'E', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
      AsciiChar(c = 'T', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
      AsciiChar(c = ' ', baf = ConsumeLine.Disposable, asciiCategory = ConsumeLine.Delimiter),
      AsciiChar(c = 'X', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
      AsciiChar(c = ' ', baf = ConsumeLine.Disposable, asciiCategory = ConsumeLine.Delimiter),
      AsciiChar(c = '=', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special),
      AsciiChar(c = '4', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit),
      AsciiChar(c = '2', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit),
      AsciiChar(c = ';', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special),
      AsciiChar(c = ' ', baf = ConsumeLine.Disposable, asciiCategory = ConsumeLine.Delimiter))
    val state = ConsumeCharState.empty[Any]
    chars.foldLeft(state) { (state, char) =>
      val (newState, strOpt) = ConsumeChars[Any, Any](char, state)
      println(strOpt)
      newState
    }
    println()

    val chars2 = Seq(
      AsciiChar(c = '2', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit),
      AsciiChar(c = '0', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit),
      AsciiChar(c = ' ', baf = ConsumeLine.Disposable, asciiCategory = ConsumeLine.Delimiter),
      AsciiChar(c = 'R', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
      AsciiChar(c = 'E', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
      AsciiChar(c = 'A', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
      AsciiChar(c = 'D', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
      AsciiChar(c = ' ', baf = ConsumeLine.Disposable, asciiCategory = ConsumeLine.Delimiter),
      AsciiChar(c = 'I', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
      AsciiChar(c = '3', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit),
      AsciiChar(c = ',', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special),
      AsciiChar(c = 'J', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
      AsciiChar(c = ',', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special),
      AsciiChar(c = 'K', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
      AsciiChar(c = '1', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit),
      AsciiChar(c = ';', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special),
      AsciiChar(c = ' ', baf = ConsumeLine.Disposable, asciiCategory = ConsumeLine.Delimiter))

    chars2.foldLeft(state) { (state, char) =>
      val (newState, strOpt) = ConsumeChars[Any, Any](char, state)
      println(strOpt)
      newState
    }
    println()
    val chars3 = Seq(
      AsciiChar(c = '3', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit),
      AsciiChar(c = '0', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit),
      AsciiChar(c = ' ', baf = ConsumeLine.Disposable, asciiCategory = ConsumeLine.Delimiter),
      AsciiChar(c = 'D', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
      AsciiChar(c = 'A', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
      AsciiChar(c = 'T', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
      AsciiChar(c = 'A', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
      AsciiChar(c = ' ', baf = ConsumeLine.Disposable, asciiCategory = ConsumeLine.Delimiter),
      AsciiChar(c = '4', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit),
      AsciiChar(c = ',', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special),
      AsciiChar(c = '-', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special),
      AsciiChar(c = '5', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit),
      AsciiChar(c = ',', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special),
      AsciiChar(c = '0', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit),
      AsciiChar(c = ';', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special),
      AsciiChar(c = ' ', baf = ConsumeLine.Disposable, asciiCategory = ConsumeLine.Delimiter))

    chars3.foldLeft(state) { (state, char) =>
      val (newState, strOpt) = ConsumeChars[Any, Any](char, state)
      println(strOpt)
      newState
    }

    println()
    val chars4 = Seq(
      AsciiChar(c = '7', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit),
      AsciiChar(c = '0', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit),
      AsciiChar(c = ' ', baf = ConsumeLine.Disposable, asciiCategory = ConsumeLine.Delimiter),
      AsciiChar(c = 'F', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
      AsciiChar(c = 'O', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
      AsciiChar(c = 'R', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
      AsciiChar(c = ' ', baf = ConsumeLine.Disposable, asciiCategory = ConsumeLine.Delimiter),
      AsciiChar(c = 'I', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
      AsciiChar(c = '=', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special),
      AsciiChar(c = '1', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit),
      AsciiChar(c = ' ', baf = ConsumeLine.Disposable, asciiCategory = ConsumeLine.Delimiter),
      AsciiChar(c = 'T', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
      AsciiChar(c = 'O', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
      AsciiChar(c = ' ', baf = ConsumeLine.Disposable, asciiCategory = ConsumeLine.Delimiter),
      AsciiChar(c = 'K', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
      AsciiChar(c = '1', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit),
      AsciiChar(c = ' ', baf = ConsumeLine.Disposable, asciiCategory = ConsumeLine.Delimiter),
      AsciiChar(c = 'S', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
      AsciiChar(c = 'T', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
      AsciiChar(c = 'E', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
      AsciiChar(c = 'P', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
      AsciiChar(c = ' ', baf = ConsumeLine.Disposable, asciiCategory = ConsumeLine.Delimiter),
      AsciiChar(c = 'J', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
      AsciiChar(c = ';', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special),
      AsciiChar(c = ' ', baf = ConsumeLine.Disposable, asciiCategory = ConsumeLine.Delimiter))

    chars4.foldLeft(state) { (state, char) =>
      val (newState, strOpt) = ConsumeChars[Any, Any](char, state)
      println(strOpt)
      newState
    }
  }
}
