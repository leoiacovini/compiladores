package automata

import automata.commons.{NumberAutomata,IdentifierAutomata, SpecialAutomata}
import org.scalatest.WordSpec

class CommonAutomataTest extends WordSpec {
  "Identifier Automata" must {
    "identify identifiers" in {
      assert(NonDeterministicFiniteAutomata.accepts(IdentifierAutomata.IdentifierAutomata, "a42bf"))
      assert(NonDeterministicFiniteAutomata.accepts(IdentifierAutomata.IdentifierAutomata, "abf"))
      assert(NonDeterministicFiniteAutomata.accepts(IdentifierAutomata.IdentifierAutomata, "a"))
      assert(!NonDeterministicFiniteAutomata.accepts(IdentifierAutomata.IdentifierAutomata, "a42bf="))
      assert(!NonDeterministicFiniteAutomata.accepts(IdentifierAutomata.IdentifierAutomata, "a42bf "))
    }
  }

  "Special Automata" must {
    "identify specials" in {
      assert(NonDeterministicFiniteAutomata.accepts(SpecialAutomata.SpecialAutomata, ">"))
      assert(NonDeterministicFiniteAutomata.accepts(SpecialAutomata.SpecialAutomata, "<"))
      assert(NonDeterministicFiniteAutomata.accepts(SpecialAutomata.SpecialAutomata, "="))

      assert(NonDeterministicFiniteAutomata.accepts(SpecialAutomata.SpecialAutomata, ";"))
      assert(NonDeterministicFiniteAutomata.accepts(SpecialAutomata.SpecialAutomata, ">="))

      assert(!NonDeterministicFiniteAutomata.accepts(SpecialAutomata.SpecialAutomata, ">=+"))
    }
  }

  "Number Automata" must {
    "identify numbers" in {
      assert(NonDeterministicFiniteAutomata.accepts(NumberAutomata.NumberAutomata, "0"))
      assert(NonDeterministicFiniteAutomata.accepts(NumberAutomata.NumberAutomata, "012"))
      assert(NonDeterministicFiniteAutomata.accepts(NumberAutomata.NumberAutomata, "123"))
      assert(!NonDeterministicFiniteAutomata.accepts(NumberAutomata.NumberAutomata, ""))
      assert(!NonDeterministicFiniteAutomata.accepts(NumberAutomata.NumberAutomata, "023d"))
    }
  }

}
