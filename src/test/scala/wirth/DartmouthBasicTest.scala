package wirth

import llvm.{LLVMProgram, OutputWriter}
import common.automata.ndpa.{NDPARunner, RunHistoryItem}
import org.scalatest.{FlatSpec, WordSpec}

class DartmouthBasicTest extends WordSpec {

  val dartmouthBasicRules = Map(
    NonTerminalToken("PROGRAM") ->
      Sequence(
        NonTerminalToken("STATEMENT"),
        ExpressionKleene(
          Sequence(
            NonTerminalToken("STATEMENT")
          )
        )
      ),
    NonTerminalToken("STATEMENT") ->
      Sequence(
        NonTerminalToken("INT"),
        NonTerminalToken("PRINT")
      ),
    NonTerminalToken("PRINT") ->
      Sequence(
        TerminalToken("PRINT"),
        NonTerminalToken("PRINTITEM"),
        TerminalToken(";"),
      ),
    NonTerminalToken("PRINTITEM") ->
      Sequence(
        TerminalToken("\""),
        NonTerminalToken("CHARACTER"),
        TerminalToken("\"")
      ),
    NonTerminalToken("CHARACTER") ->
      Or(
        TerminalToken("a"),
        TerminalToken("b")
      ),
    NonTerminalToken("INT") ->
      Sequence(
        NonTerminalToken("DIGIT"),
        ExpressionKleene(
          NonTerminalToken("DIGIT")
        )
      ),
    NonTerminalToken("DIGIT") ->
      Or(
        TerminalToken("0"),
        TerminalToken("1"),
        TerminalToken("2"),
        TerminalToken("3"),
        TerminalToken("4"),
        TerminalToken("5"),
        TerminalToken("6"),
        TerminalToken("7"),
        TerminalToken("8"),
        TerminalToken("9")
      ),
    NonTerminalToken("ASSIGNMENT") ->
      Sequence(
        TerminalToken("LET"),
        TerminalToken("id"),
        TerminalToken("="),
        NonTerminalToken("EXPRESSION")
      ),
    NonTerminalToken("EXPRESSION") ->
      Sequence(
        NonTerminalToken("TERM"),
        ExpressionKleene(
          Sequence(
            Or(
              TerminalToken("+"),
              TerminalToken("-")
            ),
            NonTerminalToken("TERM")
          )
        )
      ),
    NonTerminalToken("TERM") ->
      Sequence(
        NonTerminalToken("FACTOR"),
        ExpressionKleene(
          Sequence(
            Or(
              TerminalToken("*"),
              TerminalToken("/")
            ),
            NonTerminalToken("FACTOR")
          )
        )
      ),
    NonTerminalToken("FACTOR") ->
      Or(
        TerminalToken("id"),
        TerminalToken("num"),
        Sequence(
          TerminalToken("("),
          NonTerminalToken("EXPRESSION"),
          TerminalToken(")"),
        )
      )
  )

  val dependencyMap: Map[NonTerminalToken, Seq[NonTerminalToken]] = Map(
    NonTerminalToken("PROGRAM") -> Seq(
      NonTerminalToken("ASSIGNMENT")
    ),
    NonTerminalToken("ASSIGNMENT") -> Seq(
      NonTerminalToken("EXPRESSION")
    ),
    NonTerminalToken("PRINT") -> Seq(
      NonTerminalToken("PRINTITEM")
    ),
    NonTerminalToken("PRINTITEM") -> Seq(
      NonTerminalToken("CHARACTER")
    ),
    NonTerminalToken("CHARACTER") -> Seq(),
    NonTerminalToken("EXPRESSION") -> Seq(
      NonTerminalToken("TERM")
    ),
    NonTerminalToken("TERM") -> Seq(
      NonTerminalToken("FACTOR")
    ),
    NonTerminalToken("FACTOR") -> Seq(
      NonTerminalToken("EXPRESSION")
    )
  )
  val root = NonTerminalToken("PROGRAM")
  "Dartmouth Basic Grammar" must {
    "recognize prints" in {
    }
  }
}
