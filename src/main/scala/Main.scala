import basic.{BasicClassifier, BasicStatement, BasicToLLVM}
import consumers.ConsumeFile.ConsumeFileOutput
import consumers.ConsumeLine.ConsumeLineOutput
import consumers.{ConsumeFile, ConsumeLine}
import common.event_machine.EventMachine
import common.event_machine.EventMachine.{Event, EventResult}
import llvm.{LLVMProgram, OutputWriter}

object Main extends App {
  val lexerGrammarFilepath: String = getClass.getResource("/basic-lexer-grammar.wirth").getPath
  val transducer = TransducerBuilder.getTransducer(lexerGrammarFilepath)
  val filename: String = args(0)
  val fileConsumer = EventMachine[String, ConsumeFileOutput, Int](0, { case Event(input: String, _) =>
    val lines: Seq[ConsumeFile.ConsumeFileOutput] = ConsumeFile(input)
    lines.foreach(println)
    EventResult(lines, 0)
  })
  val lineConsumer = EventMachine[ConsumeFileOutput, ConsumeLineOutput, Int](0, { case Event(input: ConsumeFileOutput, _) =>
    val chars: Seq[ConsumeLineOutput] = ConsumeLine(input)
    EventResult(chars, 0)
  })
  val lines = fileConsumer.consume(Seq(filename))
  val chars: Seq[ConsumeLineOutput] = lineConsumer.consume(lines)
  val ascii: Seq[ConsumeLine.AsciiChar] = chars collect {case c: ConsumeLine.AsciiChar => c}
  val lexical = transducer.transduce(ascii)
  val basicProgram: Seq[BasicStatement] = BasicClassifier.tokenize(lexical)
  val llvm: LLVMProgram = BasicToLLVM.toLLVM(basicProgram)

  OutputWriter.write(llvm, filename + ".ll")
//  .foreach(println)
}
