package wirth

import automata.ndpa.NDPARunner
import org.scalatest.{FlatSpec, WordSpec}

class DartmouthBasicTest extends WordSpec {

  val dartmouthBasicRules = Map(
    NonTerminalToken("PROGRAM") ->
      Sequence(
        NonTerminalToken("PRINT"),
        ExpressionKleene(
          Sequence(
            TerminalToken(";"),
            NonTerminalToken("PRINT")
          )
        )
      ),
    NonTerminalToken("PRINT") ->
      Sequence(
        TerminalToken("PRINT"),
        NonTerminalToken("PRINTITEM")
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
      val dartmouthNDPA = WirthToNDPA.fromRules(dartmouthBasicRules, root)
      val runner = NDPARunner.fromNDPA(dartmouthNDPA)

      assert(runner.accepts(Seq(Terminal("PRINT"), Terminal("\""), Terminal("a"), Terminal("\""))))
      val run = runner.runAll(Seq(Terminal("PRINT"), Terminal("\""), Terminal("a"), Terminal("\"")))
      println(run.debugString)
    }
  }
}
