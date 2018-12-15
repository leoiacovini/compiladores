package wirth

import common.automata.{LexicalToken, Transducer}
import consumers.ConsumeLine

class WirthTransducer extends Transducer {
  override def transduce(chars: Seq[ConsumeLine.AsciiChar]): Seq[LexicalToken] = ???
}
