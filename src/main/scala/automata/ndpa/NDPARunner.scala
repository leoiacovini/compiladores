package automata.ndpa

import scala.util.Try

case class NDPARunner[InputSymbol, StackSymbol, State](ndpa: NonDeterministicPushdownAutomata[InputSymbol, StackSymbol, State],
                                                       current: Seq[NDPARunState[State, StackSymbol]]) {

  def fail(): NDPARunner[InputSymbol, StackSymbol, State] = copy(current = Seq(current.head.withState(ndpa.trapState)))


  def run(input: Option[InputSymbol]): NDPARunner[InputSymbol, StackSymbol, State] = {
    val appliedNewStates = for {
      currentRunState <- current
      transition <- ndpa.transition(currentRunState.state, input, currentRunState.stack.headOption)
    } yield currentRunState.popStack().applyTransitionResult(transition)

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
      case NDPARunState(state, stack) => ndpa.acceptStates.contains(state) && stack.isEmpty
    }
  }

}

object NDPARunner {
  def fromNDPA[InputSymbol, StackSymbol, State](ndpa: NonDeterministicPushdownAutomata[InputSymbol, StackSymbol, State]): NDPARunner[InputSymbol, StackSymbol, State] = {
    val initialCurrent = runEmptyTransitions(ndpa, Seq(NDPARunState(ndpa.initialState, Seq(ndpa.initialStackSymbol))))
    NDPARunner(ndpa, initialCurrent)
  }

  def runEmptyTransitions[InputSymbol, StackSymbol, State](ndpa: NonDeterministicPushdownAutomata[InputSymbol, StackSymbol, State], runStates: Seq[NDPARunState[State, StackSymbol]]): Seq[NDPARunState[State, StackSymbol]] = {
    runStates.flatMap { runState =>
      val emptyTransitionRunStates = ndpa.transition(runState.state, None, None).map(runState.applyTransitionResult)
      val stackTransitionRunStates = ndpa.transition(runState.state, None, runState.stack.headOption).map(runState.popStack().applyTransitionResult)

      val automaticTransitions = (emptyTransitionRunStates ++ stackTransitionRunStates).filterNot(runStates.contains).distinct
      if (automaticTransitions.nonEmpty)
        runEmptyTransitions(ndpa, runStates ++ automaticTransitions).distinct
      else runStates.distinct
    }
  }
}
