package wirth

import automata.ndpa.{NDPARunner, RunHistoryItem}
import basic.{BasicToLLVM, Print}
import llvm.{LLVMProgram, OutputWriter}
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
      val dartmouthNDPA = WirthToNDPA.fromRules(dartmouthBasicRules, root)
      val runner = NDPARunner.fromNDPA(dartmouthNDPA)

      val input = Seq(Terminal("1"), Terminal("1"), Terminal("PRINT"), Terminal("\""), Terminal("a"), Terminal("\""), Terminal(";"))
      val input2 = input ++ input
      assert(runner.accepts(input))
      assert(runner.accepts(input2))
      val run = runner.runAll(input)
      val run2 = runner.runAll(input2)
//      println(run.debugString)
      run2.getAcceptedRunState.foreach { runState =>
        println(runState.history.map(_.debugString).mkString("\n"))
      }

      val history = run2.getAcceptedRunState.map(_.history).getOrElse(Seq.empty)
      val x = run2.getAcceptedRunState.map(_.history).getOrElse(Seq.empty).map(_.inputSymbolOpt)
      val abc: Map[Int, Seq[RunHistoryItem[WirthToNDPA.WirthGeneratedState, WirthLexicalToken, WirthToNDPA.StackAlphabet]]] = history.zipWithIndex.groupBy {case (_, index) =>
        history.drop(index).count(h => h.inputSymbolOpt.contains(Terminal(";")))
      }.mapValues {x => x.map(_._1)}

      val lLVMProgram: LLVMProgram = abc.mapValues(BasicToLLVM.getCommand).toSeq.sortBy(_._1).map(_._2).foldLeft(LLVMProgram.empty) {case (llvm, print: Print) =>
        BasicToLLVM.addPrint(llvm, print)
      }

      println(lLVMProgram.toString)
      OutputWriter.write(lLVMProgram, "teste2.ll")
    }
  }
}
