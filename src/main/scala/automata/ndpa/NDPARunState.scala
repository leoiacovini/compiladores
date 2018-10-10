package automata.ndpa

import automata.SeqStack

case class NDPARunState[State, StackSymbol](state: State, stack: Seq[StackSymbol]) {
  import SeqStack._
  def withState(newState: State): NDPARunState[State, StackSymbol] = copy(state = newState)

  def popStack(): NDPARunState[State, StackSymbol] = copy(stack = new SeqStack(stack).pop())
  def applyTransitionResult(transitionResult: (State, Seq[StackSymbol])): NDPARunState[State, StackSymbol] = this.withState(transitionResult._1).copy(stack = new SeqStack(stack).push(transitionResult._2))
}
