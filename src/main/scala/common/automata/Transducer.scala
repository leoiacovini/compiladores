package common.automata

import consumers.ConsumeLine.AsciiChar

case class LexicalToken(typ: String, rawValue: String)

trait Transducer {
  def transduce(chars: Seq[AsciiChar]) : Seq[LexicalToken]
}
