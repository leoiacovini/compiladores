import basic.{BasicClassifier, BasicStatement, BasicToLLVM}
import consumers.ConsumeFile.ConsumeFileOutput
import consumers.ConsumeLine.ConsumeLineOutput
import consumers.{ConsumeFile, ConsumeLine}
import common.event_machine.EventMachine
import common.event_machine.EventMachine.{Event, EventResult}
import llvm.{LLVMProgram, OutputWriter}

import scala.io.Source

object Main extends App {
  if (args.length != 3) {
    println("Required 2 argument [basic-grammar-wirth] [program-file] [outfile]")
    throw new IllegalArgumentException()
  }
  val lexerGrammarFilepath: String = args(0)
  val transducer = TransducerBuilder.getTransducer(lexerGrammarFilepath)
  println("Transducer Created")
  val filename: String = args(1)
  val fileConsumer = EventMachine[String, ConsumeFileOutput, Int](0, { case Event(input: String, _) =>
    val lines: Seq[ConsumeFile.ConsumeFileOutput] = ConsumeFile(input)
    lines.foreach(println)
    EventResult(lines, 0)
  })
  val lineConsumer = EventMachine[ConsumeFileOutput, ConsumeLineOutput, Int](0, { case Event(input: ConsumeFileOutput, _) =>
    val chars: Seq[ConsumeLineOutput] = ConsumeLine(input)
    EventResult(chars, 0)
  })
  println("Starting Parses...")
  val lines = fileConsumer.consume(Seq(filename))
  val chars: Seq[ConsumeLineOutput] = lineConsumer.consume(lines)
  val ascii: Seq[ConsumeLine.AsciiChar] = chars collect {case c: ConsumeLine.AsciiChar => c}
  println("Starting Transducer...")
  val lexical = transducer.transduce(ascii)
  println("Starting Classification...")
  val basicProgram: Seq[BasicStatement] = BasicClassifier.tokenize(lexical)
  println("Starting LLVM Code Generation...")
  val llvm: LLVMProgram = BasicToLLVM.toLLVM(basicProgram)

  OutputWriter.write(llvm, args(2) + ".ll")
  println("DONE!")
//  .foreach(println)
}
