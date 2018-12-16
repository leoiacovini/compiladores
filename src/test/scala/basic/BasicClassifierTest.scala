package basic

import common.automata.LexicalToken
import org.scalatest.WordSpec

class BasicClassifierTest extends WordSpec {
  "BasicClassifier" must {
    "classify" in {
      val line = BasicClassifier.classifyLine(Seq(
        LexicalToken("INTEGER", "10"),
        LexicalToken("RESERVEDKEYWORD", "LET"),
        LexicalToken("VARIABLENAME", "B1"),
        LexicalToken("OPERATOR", "="),
        LexicalToken("INTEGER", "44")
      ))

      assert(line == Seq(
        BasicToken.LineNumber("10"),
        BasicToken.Let(),
        BasicToken.Identifier("B1"),
        BasicToken.Equal(),
        BasicToken.Number("44")
      ))
    }
  }
}
