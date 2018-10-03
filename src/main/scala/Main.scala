import consumers.ConsumeFile.ConsumeFileOutput
import consumers.ConsumeLine.{AsciiChar, ConsumeLineOutput}
import consumers.{ConsumeChars, ConsumeFile, ConsumeLine}
import event_machine.EventMachine
import event_machine.EventMachine.{Event, EventResult}

object Main extends App {
  val filename: String = args(0)
  val fileConsumer = EventMachine[String, ConsumeFileOutput, Int](0, { case Event(input: String, _) =>
    val lines: Seq[ConsumeFile.ConsumeFileOutput] = ConsumeFile(input)
    EventResult(lines, 0)
  })
  val lineConsumer = EventMachine[ConsumeFileOutput, ConsumeLineOutput, Int](0, { case Event(input: ConsumeFileOutput, _) =>
    val chars: Seq[ConsumeLineOutput] = ConsumeLine(input)
    EventResult(chars, 0)
  })

//  val charsConsumer = EventMachine[AsciiChar, ConsumeCharOutput, ConsumeCharState[Any]](ConsumeCharState.empty[IdentifierState], (input: AsciiChar) => {
//
//  })
}
