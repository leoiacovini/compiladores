package automata

import org.scalatest._

class NonDeterministicFiniteAutomataTest extends WordSpec {
  "NonDeterministicFiniteAutomata" must {
    val ndfa = new NonDeterministicFiniteAutomata[Char, Int] {
      override val alphabet: Set[Char] = CharAlphabets.Alphanumeric
      override val initialState: Int = 0
      override val states: Set[Int] = Set(0, 1, 2, 3, 4)
      override val acceptStates: Set[Int] = Set(3)

      override def transition(state: Int, symbol: Char): Set[Int] = {
        (state, symbol) match {
          case (0, 'a') => Set(1)
          case (1, 'b') => Set(2)
          case (2, 'c') => Set(3)
          case (3, 'c') => Set(3)
          case _ => Set(4)
        }
      }
    }

    val ndfa2 = new NonDeterministicFiniteAutomata[Char, Int] {
      override val alphabet: Set[Char] = CharAlphabets.Alphanumeric
      override val initialState: Int = 5
      override val states: Set[Int] = Set(6, 7, 8, 9)
      override val acceptStates: Set[Int] = Set(8)

      override def transition(state: Int, symbol: Char): Set[Int] = {
        (state, symbol) match {
          case (5, 'd') => Set(6)
          case (6, 'e') => Set(7)
          case (7, 'f') => Set(8)
          case _ => Set(9)
        }
      }
    }
    "accept sequences abc*" in {
      assert(NonDeterministicFiniteAutomata.accepts(ndfa, "abc"))
      assert(NonDeterministicFiniteAutomata.accepts(ndfa, "abcc"))
      assert(NonDeterministicFiniteAutomata.accepts(ndfa, "abccc"))
      assert(!NonDeterministicFiniteAutomata.accepts(ndfa, "abbccc"))
    }

    "accept sequence def" in {
      assert(NonDeterministicFiniteAutomata.accepts(ndfa2, "def"))
      assert(!NonDeterministicFiniteAutomata.accepts(ndfa2, "deff"))
    }

    "concat automatons into abc*def" in {
      val newNDFA = NonDeterministicFiniteAutomata.concat(ndfa, ndfa2)

      assert(NonDeterministicFiniteAutomata.accepts(newNDFA, "abcdef"))
      assert(NonDeterministicFiniteAutomata.accepts(newNDFA, "abccdef"))
      assert(!NonDeterministicFiniteAutomata.accepts(newNDFA, "abddef"))
    }
  }
}
