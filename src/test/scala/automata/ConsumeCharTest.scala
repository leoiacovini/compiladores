package automata

import automata.commons.IdentifierAutomata.IdentifierState
import consumers.ConsumeChars.ConsumeCharState
import consumers.{ConsumeChars, ConsumeLine}
import consumers.ConsumeLine.AsciiChar
import org.scalatest.WordSpec

class ConsumeCharTest extends WordSpec {
  "foo" in {
    val a = AsciiChar(c = 'c', baf = ConsumeLine.Useful, asciiCategory = ConsumeLine.Letter)
    val state = ConsumeCharState.empty[IdentifierState]
    println(ConsumeChars.apply(a, state))
  }
}
