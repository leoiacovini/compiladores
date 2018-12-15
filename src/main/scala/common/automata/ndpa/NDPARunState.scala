package common.automata.ndpa

import common.automata.SeqStack

case class RunHistoryItem[State, InputSymbol, StackSymbol](fromState: State, toState: State,
                                                           newStackSymbols: Seq[StackSymbol],
                                                           inputSymbolOpt: Option[InputSymbol],
                                                           stackSymbolOpt: Option[StackSymbol]) {
  def debugString: String = {
    s"""$fromState, $inputSymbolOpt, $stackSymbolOpt
            -> $toState, $newStackSymbols"""
  }
}
case class NDPARunState[State, InputSymbol, StackSymbol](state: State, stack: Seq[StackSymbol], history: Seq[RunHistoryItem[State, InputSymbol, StackSymbol]]) {
  import SeqStack._

  override def equals(obj: Any): Boolean = obj match {
    case ndpa: NDPARunState[State, InputSymbol, StackSymbol] =>
      ndpa.state == this.state &&
        ndpa.stack == this.stack
    case _ => false
  }
  def withState(newState: State): NDPARunState[State, InputSymbol, StackSymbol] = copy(state = newState)
  def pushHistoryItem(runHistoryItem: RunHistoryItem[State, InputSymbol, StackSymbol]): NDPARunState[State, InputSymbol, StackSymbol] = copy(history = history :+ runHistoryItem)
  def popStack(): NDPARunState[State, InputSymbol, StackSymbol] = copy(stack = new SeqStack(stack).pop())
  def applyTransitionResult(inputSymbolOpt:Option[InputSymbol], stackSymbolOpt: Option[StackSymbol], transitionResult: (State, Seq[StackSymbol])): NDPARunState[State, InputSymbol, StackSymbol] =
    this
      .withState(transitionResult._1)
      .copy(stack = new SeqStack(stack).push(transitionResult._2))
      .pushHistoryItem(RunHistoryItem(state, transitionResult._1, transitionResult._2, inputSymbolOpt, stackSymbolOpt))
}
