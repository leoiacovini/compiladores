import java.io.File

import common.automata.Transducer
import common.event_machine.EventMachine
import common.event_machine.EventMachine.{Event, EventResult}
import consumers.ConsumeFile.ConsumeFileOutput
import consumers.{ConsumeChars, ConsumeFile, ConsumeLine}
import consumers.ConsumeLine.ConsumeLineOutput
import wirth._

import scala.io.Source

object TransducerBuilder {
  def getTransducer(lexerGrammarFilepath: String): Transducer = {

    val fileConsumer = EventMachine[String, ConsumeFileOutput, Int](0, { case Event(input: String, _) =>
      val lines: Seq[ConsumeFile.ConsumeFileOutput] = ConsumeFile(input)
      EventResult(lines, 0)
    })
    val lineConsumer = EventMachine[ConsumeFileOutput, ConsumeLineOutput, Int](0, { case Event(input: ConsumeFileOutput, _) =>
      val chars: Seq[ConsumeLineOutput] = ConsumeLine(input)
      EventResult(chars, 0)
    })
    val lines = fileConsumer.consume(Seq(lexerGrammarFilepath))
    val chars: Seq[ConsumeLineOutput] = lineConsumer.consume(lines)
    val ascii: Seq[ConsumeLine.AsciiChar] = chars collect {case c: ConsumeLine.AsciiChar => c}
    val transducer = new WirthTransducer
    val lexical = transducer.transduce(ascii)
    val wirthLex = transducer.transform(lexical)
    val grammar: Map[NonTerminalToken, Expression] = WirthExperimentation.parseGrammar(wirthLex)
    WirthExperimentation.printRules(grammar)
    println(grammar(NonTerminalToken("VARIABLENAME")))
    val value = grammar(NonTerminalToken("STRING")) match { case s: Sequence => s.expressions.collect {case t: TerminalToken => t.terminal.str}}
    println(value)
    println(value.map(_.length))
    WirthLexer.buildTransducer(grammar, Seq(
      NonTerminalToken("INTEGER"),
      NonTerminalToken("STRING"),
      NonTerminalToken("VARIABLENAME"),
      NonTerminalToken("RESERVEDKEYWORD"),
      NonTerminalToken("EXPRESSIONOPERATION"),
      NonTerminalToken("OPERATOR"),
      NonTerminalToken("ENDOFSTATEMENT")
    ))
  }
}
