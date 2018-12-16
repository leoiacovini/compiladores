package wirth

import common.automata.LexicalToken
import consumers.ConsumeLine
import consumers.ConsumeLine.AsciiChar
import org.scalatest.WordSpec

class WirthTransducerTest extends WordSpec {
  val transducer = new WirthTransducer
  "A WirthTransducer's transduce" must {
    "parse rule correctly" in {
      val lexicalTokens = transducer.transduce(
        Seq(
          AsciiChar(c = 'A', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
          AsciiChar(c = '-', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special),
          AsciiChar(c = '>', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special),
          AsciiChar(c = '"', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special),
          AsciiChar(c = '3', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit),
          AsciiChar(c = '"', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special),
          AsciiChar(c = '.', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special)
        )
      )
      assert(lexicalTokens == Seq(
        LexicalToken("NonTerminal", "A"),
        LexicalToken("OtherSymbols", "->"),
        LexicalToken("Terminal", "\"3\""),
        LexicalToken("OtherSymbols", ".")
      ))
    }

    "ignore spaces" in {
      val lexicalTokens = transducer.transduce(
        Seq(
          AsciiChar(c = 'A', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
          AsciiChar(c = ' ', baf = ConsumeLine.Disposable, asciiCategory = ConsumeLine.Delimiter),
          AsciiChar(c = '-', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special),
          AsciiChar(c = '>', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special),
          AsciiChar(c = '"', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special),
          AsciiChar(c = '3', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit),
          AsciiChar(c = '"', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special),
          AsciiChar(c = '.', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special)
        )
      )
      assert(lexicalTokens == Seq(
        LexicalToken("NonTerminal", "A"),
        LexicalToken("OtherSymbols", "->"),
        LexicalToken("Terminal", "\"3\""),
        LexicalToken("OtherSymbols", ".")
      ))
    }
  }

  "WirthTransducer's transform" must {
    "convert to object WirthLexicalToken" in {
      val wlt = transducer.transform(Seq(
        LexicalToken("NonTerminal", "A"),
        LexicalToken("OtherSymbols", "->"),
        LexicalToken("Terminal", "\"3\""),
        LexicalToken("OtherSymbols", ".")
      ))

      assert(wlt == Seq(
        NonTerminal("A"),
        Arrow,
        Terminal("3"),
        Special(".")
      ))
    }
  }
}
