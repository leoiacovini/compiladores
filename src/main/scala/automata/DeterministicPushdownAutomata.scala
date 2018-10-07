package automata

import scala.util.{Failure, Success, Try}

trait DeterministicPushdownAutomata[InputSymbol, StackSymbol, +State] {
  val inputAlphabet: Seq[InputSymbol]
  val stackAlphabet: Seq[StackSymbol]
  val initialStackSymbol: StackSymbol
  val initialState: State
  val states: Seq[State]
  val acceptStates: Seq[State]
  val trapState: State
  def accepts[S >: State](state: S): Boolean = acceptStates.contains(state)
  def transition[S >: State](state: S, inputSymbolOpt: Option[InputSymbol], stackSymbol: StackSymbol): (S, Seq[StackSymbol])
}

case class DPARunner[InputSymbol ,StackSymbol, State](dpa: DeterministicPushdownAutomata[InputSymbol, StackSymbol, State],
                                                      currentState: State,
                                                      stack: Seq[StackSymbol]) {

  def fail(): DPARunner[InputSymbol, StackSymbol, State] = copy(currentState = dpa.trapState)
  def run(input: Option[InputSymbol]): DPARunner[InputSymbol, StackSymbol, State] = {
    if(stack.isEmpty) return fail()
    print {
      s"""
        |currentState: $currentState
        |stack: $stack
        |stackTop: ${stack.head}
        |input: $input""".stripMargin
    }
    val (newState, newStackSymbols) = dpa.transition(currentState, input, stack.head)

    print {
      s"""
         |newState: $newState
         |newStackSymbols: $newStackSymbols
         |newStack: ${newStackSymbols ++ stack.drop(1)}""".stripMargin
    }
    val newRunner = copy(currentState = newState, stack = newStackSymbols ++ stack.drop(1))
    println {
      s"""
         |runNone?: ${newRunner != this && newRunner.stack.nonEmpty}
      """.stripMargin
    }
    if(newRunner != this && newRunner.stack.nonEmpty)
      newRunner.run(None)
    else newRunner
  }

  def runAll(seqInput: Seq[InputSymbol]): DPARunner[InputSymbol, StackSymbol, State] = {
    println(seqInput)
    seqInput.foldLeft(this) {
      case (runner, input) =>
        println(input + " -> " + runner)
        runner.run(Some(input))
    }
  }
  def accepts(symbols: Seq[InputSymbol]): Boolean = {
    runAll(symbols).isAccepted
  }

  def rejects(symbols: Seq[InputSymbol]): Boolean = !accepts(symbols)
  def isStateAccepted: Boolean = dpa.accepts(currentState)
  def isStackEmpty: Boolean = stack.isEmpty
  def isStackWithOnlyInitialSymbol: Boolean = stack == Seq(dpa.initialStackSymbol)

  def isAccepted: Boolean = isStateAccepted && (isStackEmpty || isStackWithOnlyInitialSymbol)

}

object DPARunner {
  def fromDPA[InputSymbol ,StackSymbol, State](dpa: DeterministicPushdownAutomata[InputSymbol ,StackSymbol, State]): DPARunner[InputSymbol ,StackSymbol, State] = {
    DPARunner(dpa, dpa.initialState, Seq(dpa.initialStackSymbol))
  }
}
