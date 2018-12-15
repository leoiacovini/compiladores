package consumers

import common.automata.commons.{IdentifierAutomata, NumberAutomata, SpecialAutomata}
import common.automata.ndfa.NDFARunner
import common.Token
import consumers.ConsumeLine._
import common.event_machine.EventMachine.{Event, EventResult}

object ConsumeChars {

  type ConsumeCharEvent = Event[AsciiChar, ConsumeCharState]
  type ConsumeCharEventResult = EventResult[Token, ConsumeCharState]
  type CharNDFARunner = NDFARunner[Char, Any]

  case class ConsumeCharState(ndfaRunner: Option[CharNDFARunner], accumulator: String = "") {
    def push(c: Char): ConsumeCharState = copy(accumulator = accumulator + c)
    def run(c: Char): ConsumeCharState = ndfaRunner match {
      case Some(ndfaRunv) => ConsumeCharState(Some(NDFARunner(ndfaRunv.ndfa, ndfaRunv.run(c))), accumulator).push(c)
      case None => this
    }
  }

  object ConsumeCharState {
    def empty: ConsumeCharState = ConsumeCharState(None)
  }

  private def runnerForAscii(ascii: AsciiChar): Option[CharNDFARunner] = {
    ascii.asciiCategory match {
      case Letter => Some(NDFARunner(IdentifierAutomata, IdentifierAutomata.initialStates))
      case Delimiter => None
      case Digit => Some(NDFARunner(NumberAutomata, NumberAutomata.initialStates))
      case Special => Some(NDFARunner(SpecialAutomata, SpecialAutomata.initialStates))
    }
  }

  private def stateForEvent(event: ConsumeCharEvent): ConsumeCharState = ConsumeCharState(runnerForAscii(event.input))

  private def startNewNdfa(event: ConsumeCharEvent): ConsumeCharEventResult = {
    runnerForAscii(event.input) match {
      case Some(runner) => processNdfa(event.copy(state = ConsumeCharState(Some(runner))))
      case None => EventResult.single(Token(event.state.accumulator), event.state)
    }
  }

  private def processNdfa(event: ConsumeCharEvent): ConsumeCharEventResult = {
    val newState: ConsumeCharState = event.state.run(event.input.c)
    if (event.state.ndfaRunner.forall(x => x.isAccepted) && !newState.ndfaRunner.forall(x => x.isAccepted)) {
      EventResult.single(Token(event.state.accumulator), stateForEvent(event).run(event.input.c))
    } else {
      EventResult.empty(newState) // Empty output
    }
  }

  def apply(event: ConsumeCharEvent): ConsumeCharEventResult = {
    event.state.ndfaRunner match {
      // If are in the middle of a automate cycle
      case Some(_: NDFARunner[Char, _]) => processNdfa(event)
      // It's the first chars os a possible sequence, we need to start a new common.automata
      case None => startNewNdfa(event)
    }
  }
}
