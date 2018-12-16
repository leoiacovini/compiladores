package common.automata.ndpa

import wirth.{Sequence, Terminal, TerminalToken}
import wirth.WirthToNDPA.AcceptSequence

import scala.util.Try

case class NDPARunner[InputSymbol, StackSymbol, State](ndpa: NonDeterministicPushdownAutomata[InputSymbol, StackSymbol, State],
                                                       current: Seq[NDPARunState[State, InputSymbol, StackSymbol]]) {

  def fail(): NDPARunner[InputSymbol, StackSymbol, State] = copy(current = Seq.empty)


  def run(input: Option[InputSymbol]): NDPARunner[InputSymbol, StackSymbol, State] = {
    val appliedNewStates: Seq[NDPARunState[State, InputSymbol, StackSymbol]] = for {
      currentRunState <- current
      transition <- ndpa.transition(currentRunState.state, input, currentRunState.stack.headOption)
    } yield currentRunState.popStack().applyTransitionResult(input, currentRunState.stack.headOption, transition)

    copy(current = NDPARunner.runEmptyTransitions(ndpa, appliedNewStates.distinct).distinct)
  }

  def runAll(seqInput: Seq[InputSymbol]): NDPARunner[InputSymbol, StackSymbol, State] = {
    seqInput.foldLeft(this) {
      case (runner, input) =>
//        println(s"$input -> $runner")
        runner.run(Some(input))
    }
  }

  def accepts(symbols: Seq[InputSymbol]): Boolean = {
    runAll(symbols).isAccepted
  }

  def rejects(symbols: Seq[InputSymbol]): Boolean = !accepts(symbols)

  def isAccepted: Boolean = {
    current.exists {
      case NDPARunState(state, stack, _) => ndpa.acceptStates.contains(state) && stack.isEmpty
    }
  }

  def getAcceptedRunState: Option[NDPARunState[State, InputSymbol, StackSymbol]] = {
    current.find {
      case NDPARunState(state, stack, _) => ndpa.acceptStates.contains(state) && stack.isEmpty
    }
  }

  def debugString: String = {
    if (this.current.isEmpty) {
      "Empty current state"
    } else {
      current.map { runState =>
        s"""
           |NDPARunState: {
           |  state: ${runState.state}
           |  stack:
           |    ${runState.stack.mkString("\n    ")}
           |  history:
           |    ${runState.history.map(_.debugString).mkString("\n    ")}
           |}
           |""".stripMargin
      }.mkString("")
    }
  }
}

object NDPARunner {
  def fromNDPA[InputSymbol, StackSymbol, State](ndpa: NonDeterministicPushdownAutomata[InputSymbol, StackSymbol, State]): NDPARunner[InputSymbol, StackSymbol, State] = {
    val initialCurrent = runEmptyTransitions(ndpa, Seq(NDPARunState[State, InputSymbol, StackSymbol](ndpa.initialState, Seq(ndpa.initialStackSymbol), Seq.empty)))
    NDPARunner(ndpa, initialCurrent)
  }

  def runEmptyTransitions[InputSymbol, StackSymbol, State](ndpa: NonDeterministicPushdownAutomata[InputSymbol, StackSymbol, State],
                                                           runStates: Seq[NDPARunState[State, InputSymbol, StackSymbol]]): Seq[NDPARunState[State, InputSymbol, StackSymbol]] = {

//    println(s"runStates: ${runStates.size}")
    runStates.flatMap { runState =>
      val emptyTransitionRunStates = ndpa.transition(runState.state, None, None).map(tr => runState.applyTransitionResult(None, None, tr))
      val stackTransitionRunStates = ndpa.transition(runState.state, None, runState.stack.headOption).map(tr => runState.popStack().applyTransitionResult(None, runState.stack.headOption, tr))
//      println {
//        s"""
//          |empty: ${emptyTransitionRunStates.size}
//          |stack: ${stackTransitionRunStates.size}
//        """.stripMargin
//      }
//      if(runState.state.equals(AcceptSequence(Sequence(TerminalToken("a"))))) {
//        println(emptyTransitionRunStates)
//      }
//      println {
//        s"""
//           |NDPARunState: {
//           |  state: ${runState.state}
//           |  stack:
//           |    ${runState.stack.mkString("\n    ")}
//           |  history:
//           |    ${runState.history.map(_.debugString).mkString("\n    ")}
//           |}
//      """.stripMargin
//      }

      val automaticTransitions = (emptyTransitionRunStates ++ stackTransitionRunStates).filterNot(runStates.contains).distinct
      if (automaticTransitions.nonEmpty)
        runEmptyTransitions(ndpa, runStates ++ automaticTransitions).distinct
      else Seq(runState)
    }.distinct
  }
}
