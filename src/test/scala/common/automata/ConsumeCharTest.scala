package common.automata

import common.Token
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

      assert(eventMachine.consume(chars) ==
        Seq(Token("10"), Token("LET"), Token("X"), Token("="), Token("42"), Token(";")))

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

      assert(eventMachine.consume(chars2) ==
        Seq(Token("20"), Token("READ"), Token("I3"), Token(","), Token("J"), Token(","), Token("K1"), Token(";")))

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

      assert(eventMachine.consume(chars3) ==
        Seq(Token("30"), Token("DATA"), Token("4"), Token(","), Token("-"), Token("5"), Token(","), Token("0"), Token(";")))

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

      assert(eventMachine.consume(chars4) ==
        Seq(Token("70"), Token("FOR"), Token("I"), Token("="), Token("1"), Token("TO"), Token("K1"), Token("STEP"), Token("J"), Token(";")))
    }
  }

}
