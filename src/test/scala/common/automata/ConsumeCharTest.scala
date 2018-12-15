package common.automata

import consumers.ConsumeChars.ConsumeCharState
import consumers.{ConsumeChars, ConsumeLine}
import consumers.ConsumeLine.AsciiChar
import common.event_machine.EventMachine
import org.scalatest.WordSpec

class ConsumeCharTest extends WordSpec {

  val eventMachine = EventMachine(ConsumeCharState.empty, ConsumeChars.apply)

  "ConsumeChars" must {
    "accept AsciiChar sequences" in {
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

      assert(eventMachine.consume(chars) == Seq("10", "LET", "X", "=", "42", ";"))

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

      assert(eventMachine.consume(chars2) == Seq("20", "20", "20", "20", "20", ",", "K1", ";"))
    }
  }

}
