package consumers

import automata.NonDeterministicFiniteAutomata
import automata.commons.IdentifierAutomata.IdentifierState
import automata.commons.{IdentifierAutomata, NumberAutomata, SpecialAutomata}
import consumers.ConsumeLine._
case class ConsumeCharNDFA[T >: NonDeterministicFiniteAutomata[_, _]](ndfa: T)
class NDFAState[T]
object ConsumeChars {

  case class NDFARun[Input, State](ndfa: NonDeterministicFiniteAutomata[Input, State], states: Set[State]) {
    def run(input: Input): Set[State] = {
      for {
        state <- states
        newStates <- ndfa.transition(state, input)
      } yield newStates
    }
    def isAccepted: Boolean = {
      states.exists(ndfa.acceptStates)
    }
  }

  case class ConsumeCharOutput()

  case class ConsumeCharState[State](ndfaRun: Option[NDFARun[Char, State]], accumulator: String = "") {
    def push(c: Char): ConsumeCharState[State] = copy(accumulator = accumulator + c)
    def run(c: Char): ConsumeCharState[State] = ndfaRun match {
      case Some(ndfaRunv) => ConsumeCharState(Some(NDFARun(ndfaRunv.ndfa, ndfaRunv.run(c)))).push(c)
      case None => this
    }
  }
  object ConsumeCharState {
    def empty[State]: ConsumeCharState[State] = ConsumeCharState[State](None)
  }
  def apply[State, NewState](char: AsciiChar, state: ConsumeCharState[State]): (ConsumeCharState[NewState], Option[String]) = {
    state.ndfaRun match {
      case Some(id: NDFARun[Char, IdentifierState]) if id.isInstanceOf[NDFARun[Char, IdentifierState]] =>
        val newState: ConsumeCharState[State] = state.run(char.c)
        if (id.isAccepted && !newState.ndfaRun.forall(x => x.isAccepted)) {
          ConsumeCharState.empty[NewState] -> Some(newState.accumulator)
        } else {
          newState.asInstanceOf[ConsumeCharState[NewState]] -> None
        }
      case None =>
        char.asciiCategory match {
          case Letter =>
            val run = NDFARun(IdentifierAutomata.IdentifierAutomata, Set(IdentifierAutomata.IdentifierAutomata.initialState))
            apply(char, ConsumeCharState(Some(run)))
          case Delimiter =>
            ConsumeCharState.empty[NewState] -> Some(state.accumulator)
          case Digit =>
            val run = NDFARun(NumberAutomata.NumberAutomata, Set(NumberAutomata.NumberAutomata.initialState))
            apply(char, ConsumeCharState(Some(run)))
          case Special =>
            val run = NDFARun(SpecialAutomata.SpecialAutomata, Set(SpecialAutomata.SpecialAutomata.initialState))
            apply(char, ConsumeCharState(Some(run)))
        }
    }
  }
}
