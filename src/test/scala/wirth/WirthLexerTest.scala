package wirth

import common.automata.LexicalToken
import common.automata.ndpa.NDPARunner
import consumers.ConsumeLine
import consumers.ConsumeLine.AsciiChar
import org.scalatest.WordSpec
import wirth.WirthToNDPA.ExpressionContext

class WirthLexerTest extends WordSpec {

  val basicRules = Map(
    NonTerminalToken("VARIABLENAME") -> Sequence(NonTerminalToken("LETTER"), ExpressionBrackets(NonTerminalToken("DIGIT"))),
    NonTerminalToken("EXPRESSIONOPERATION") -> Or(
      TerminalToken("+"),
      TerminalToken("-"),
      TerminalToken("/"),
      TerminalToken("*")
    ),
    NonTerminalToken("OPERATOR") -> Or(
      TerminalToken("="),
      TerminalToken(">"),
      TerminalToken("<"),
      Sequence(TerminalToken(">"), TerminalToken("=")),
      Sequence(TerminalToken("<"), TerminalToken("=")),
      Sequence(TerminalToken("<"), TerminalToken(">"))
    ),
    NonTerminalToken("RESERVEDKEYWORD") -> Or(
      Sequence("LET".map(c => TerminalToken(new String(Array(c)))): _*),
      Sequence("RETURN".map(c => TerminalToken(new String(Array(c)))): _*),
      Sequence("GOSUB".map(c => TerminalToken(new String(Array(c)))): _*),
      Sequence("READ".map(c => TerminalToken(new String(Array(c)))): _*),
      Sequence("DATA".map(c => TerminalToken(new String(Array(c)))): _*),
      Sequence("PRINT".map(c => TerminalToken(new String(Array(c)))): _*),
      Sequence("FOR".map(c => TerminalToken(new String(Array(c)))): _*),
      Sequence("NEXT".map(c => TerminalToken(new String(Array(c)))): _*),
      Sequence("REM".map(c => TerminalToken(new String(Array(c)))): _*),
      Sequence("THEN".map(c => TerminalToken(new String(Array(c)))): _*),
      Sequence("FN".map(c => TerminalToken(new String(Array(c)))): _*),
      Sequence("STEP".map(c => TerminalToken(new String(Array(c)))): _*),
      Sequence("TO".map(c => TerminalToken(new String(Array(c)))): _*),
      Sequence("DIM".map(c => TerminalToken(new String(Array(c)))): _*),
      Sequence("END".map(c => TerminalToken(new String(Array(c)))): _*),
      Sequence("DEF".map(c => TerminalToken(new String(Array(c)))): _*)
    ),
    NonTerminalToken("STRING") -> Sequence(
      TerminalToken("\""),
      ExpressionKleene(NonTerminalToken("LETTER")),
      TerminalToken("\"")
    ),
    NonTerminalToken("INTEGER") -> Sequence(
      NonTerminalToken("DIGIT"),
      ExpressionKleene(NonTerminalToken("DIGIT"))
    ),
    NonTerminalToken("LETTER") -> Or(
      (('A' to 'Z') ++ ('a' to 'z')).map(c => TerminalToken(new String(Array(c)))): _*
    ),
    NonTerminalToken("DIGIT") -> Or(
      TerminalToken("0"),
      TerminalToken("1"),
      TerminalToken("2"),
      TerminalToken("3"),
      TerminalToken("4"),
      TerminalToken("5"),
      TerminalToken("6"),
      TerminalToken("7"),
      TerminalToken("8"),
      TerminalToken("9"))
  )

  val lexicalNonTerminals = Seq(
    NonTerminalToken("INTEGER"),
    NonTerminalToken("STRING"),
    NonTerminalToken("VARIABLENAME"),
    NonTerminalToken("RESERVEDKEYWORD"),
    NonTerminalToken("EXPRESSIONOPERATION"),
    NonTerminalToken("OPERATOR"))
  "WordLexer's buildTransducer" must {
    "create a transducer for identifier" in {
      val rules = Map(
        NonTerminalToken("ID") -> Sequence(NonTerminalToken("LETTER"), NonTerminalToken("DIGIT")),
        NonTerminalToken("LETTER") -> Or(TerminalToken("A"), TerminalToken("B"), TerminalToken("C")),
        NonTerminalToken("DIGIT") -> Or(TerminalToken("0"), TerminalToken("1"), TerminalToken("2"), TerminalToken("3"), TerminalToken("4"))
      )
      val chars = Seq(
        AsciiChar(c = 'A', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
        AsciiChar(c = '0', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit),
        AsciiChar(c = ' ', baf = ConsumeLine.Disposable, asciiCategory = ConsumeLine.Delimiter),
        AsciiChar(c = 'B', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
        AsciiChar(c = '1', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit),
        AsciiChar(c = ' ', baf = ConsumeLine.Disposable, asciiCategory = ConsumeLine.Delimiter),
        AsciiChar(c = 'C', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
        AsciiChar(c = '4', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit)
      )

      val transducer = WirthLexer.buildTransducer(
        rules = rules,
        lexicalNonTerminals = Seq(NonTerminalToken("ID")))

      assert(transducer.transduce(chars) == Seq(
        LexicalToken("ID", "A0"),
        LexicalToken("ID", "B1"),
        LexicalToken("ID", "C4")
      ))
    }

    "create a transducer for basic" in {

      val chars = Seq(
        AsciiChar(c = '1', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit),
        AsciiChar(c = '0', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit),
        AsciiChar(c = ' ', baf = ConsumeLine.Disposable, asciiCategory = ConsumeLine.Delimiter),
        AsciiChar(c = 'B', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
        AsciiChar(c = '1', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit),
        AsciiChar(c = 'F', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
        AsciiChar(c = 'O', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
        AsciiChar(c = 'R', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
        AsciiChar(c = ' ', baf = ConsumeLine.Disposable, asciiCategory = ConsumeLine.Delimiter),
        AsciiChar(c = '1', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit),
        AsciiChar(c = '4', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit),
        AsciiChar(c = '>', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special),
        AsciiChar(c = '4', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit),
        AsciiChar(c = '4', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit)
      )

      val transducer = WirthLexer.buildTransducer(
        rules = basicRules,
        lexicalNonTerminals = lexicalNonTerminals)

      assert(transducer.transduce(chars) == Seq(
        LexicalToken("INTEGER", "10"),
        LexicalToken("VARIABLENAME", "B1"),
        LexicalToken("RESERVEDKEYWORD", "FOR"),
        LexicalToken("INTEGER", "14"),
        LexicalToken("OPERATOR", ">"),
        LexicalToken("INTEGER", "44")
      ))
    }

    "transducing print" in {

      val chars = Seq(
        AsciiChar(c = '1', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit),
        AsciiChar(c = '0', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Digit),
        AsciiChar(c = ' ', baf = ConsumeLine.Disposable, asciiCategory = ConsumeLine.Delimiter),
        AsciiChar(c = 'P', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
        AsciiChar(c = 'R', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
        AsciiChar(c = 'I', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
        AsciiChar(c = 'N', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
        AsciiChar(c = 'T', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
        AsciiChar(c = ' ', baf = ConsumeLine.Disposable, asciiCategory = ConsumeLine.Delimiter),
        AsciiChar(c = '"', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special),
        AsciiChar(c = 'F', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
        AsciiChar(c = 'O', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
        AsciiChar(c = 'O', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter),
        AsciiChar(c = '"', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Special)
      )

      val transducer = WirthLexer.buildTransducer(
        rules = basicRules,
        lexicalNonTerminals = lexicalNonTerminals)

      assert(transducer.transduce(chars) == Seq(
        LexicalToken("INTEGER", "10"),
        LexicalToken("RESERVEDKEYWORD", "PRINT"),
        LexicalToken("STRING", "\"FOO\"")
      ))
    }
  }
}
