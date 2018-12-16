package wirth

import common.automata.{LexicalToken, Transducer}
import common.automata.ndfa.NDFARunner
import common.automata.ndpa.NDPARunner
import consumers.ConsumeLine
import consumers.ConsumeLine.AsciiChar

object WirthLexer {
  type GeneratedRunner = NDPARunner[WirthLexicalToken, WirthToNDPA.StackAlphabet, WirthToNDPA.WirthGeneratedState]

  def getNext(before: GeneratedRunner,
              after: GeneratedRunner,
              reset: GeneratedRunner,
              typ: String,
              beforeRawValue: String): (GeneratedRunner, Option[LexicalToken]) = {
    (before, after) match {
      case (b, a) if b.isAccepted && !a.isAccepted => (reset, Some(LexicalToken(typ, beforeRawValue)))
      case (b, a) if b.isAccepted => (a, Some(LexicalToken(typ, beforeRawValue)))
      case (_, a) => (a, None)
    }
  }

  def run(state: Map[NonTerminalToken, GeneratedRunner],
          chars: Seq[ConsumeLine.AsciiChar]): Map[NonTerminalToken, GeneratedRunner] = {
    state.mapValues { genRunner =>
      val codeAsTerminals = chars.map(c => Terminal(new String(Array(c.c))))
      genRunner.runAll(codeAsTerminals)
    }
  }

  def printState(state: Map[NonTerminalToken, GeneratedRunner]): Unit = {
    state.foreach {case (ntt, runner) =>
      println(s"${ntt.nonTerminal.str} -> ${runner.isAccepted}")
    }
  }

  def getNextTransducableSegment(chars: Seq[AsciiChar]): Seq[AsciiChar] = {
    chars
      .dropWhile(c => c.baf != ConsumeLine.Useful)
      .takeWhile(c => c.baf == ConsumeLine.Useful)
  }

  def buildTransducer(rules: Map[NonTerminalToken, Expression],
                      lexicalNonTerminals: Seq[NonTerminalToken]): Transducer = {

    val initialState: Map[NonTerminalToken, GeneratedRunner] = lexicalNonTerminals.map(lnt => lnt -> NDPARunner.fromNDPA(WirthToNDPA.fromRules(rules, lnt))).toMap
    new Transducer {
      override def transduce(chars: Seq[AsciiChar]): Seq[LexicalToken] = {
        println(s"Transduce on $chars")
        val transduceChars = getNextTransducableSegment(chars)
        (0 to transduceChars.size).foldLeft((initialState, Option.empty[LexicalToken])) { case ((before, lexical), index) =>
          val nextValue = transduceChars.slice(0, index + 1)
          println(s"nextValue: $nextValue")
          val beforeRawValue = new String(transduceChars.slice(0, index).filter(_.baf == ConsumeLine.Useful).map(_.c).toArray)

          val running = for {
            (ntt, beforeGenRunner) <- before
            (ntt2, afterGenRunner) <- run(initialState, nextValue) if ntt2 == ntt
            initialGenRunner <- initialState.get(ntt)
          } yield {
            val (nextRunner, lexicalTokenOpt): (GeneratedRunner, Option[LexicalToken]) = getNext(beforeGenRunner, afterGenRunner, initialGenRunner, ntt.nonTerminal.str, beforeRawValue)
            ntt -> (nextRunner, lexicalTokenOpt)
          }
          val nextState: Map[NonTerminalToken, GeneratedRunner] = running.mapValues(_._1)
          printState(nextState)
          val nextLexical: Option[LexicalToken] = (running.values.map(_._2) ++ Seq(lexical)).toSeq.sortBy(x => x.map(_.rawValue.length).getOrElse(0)).reverse.collectFirst { case Some(lt) => lt }
          println(nextLexical)
          (nextState, nextLexical)
        } match {
          case (_, Some(lexicalToken)) =>
            Seq(lexicalToken) ++ this.transduce(chars.dropWhile(c => c.baf != ConsumeLine.Useful).drop(lexicalToken.rawValue.length))
          case (_, None) if transduceChars.isEmpty => Seq.empty
          case (_, None) => throw new Exception(s"Could not find any token in $chars")
        }
      }
    }

  }
}
