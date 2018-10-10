package automata

import automata.ndfa.NonDeterministicFiniteAutomata
import org.scalatest._

class NonDeterministicFiniteAutomataTest extends WordSpec {
  "NonDeterministicFiniteAutomata" must {
    val ndfa = new NonDeterministicFiniteAutomata[Char, Int] {
      override val alphabet: Seq[Char] = CharAlphabets.Alphanumeric.toSeq
      override val initialStates: Seq[Int] = Seq(0)
      override val states: Seq[Int] = Seq(0, 1, 2, 3, 4)
      override val acceptStates: Seq[Int] = Seq(3)

      override def transition[S >: Int](state: S, symbol: Char): Seq[Int] = {
        (state, symbol) match {
          case (0, 'a') => Seq(1)
          case (1, 'b') => Seq(2)
          case (2, 'c') => Seq(3)
          case (3, 'c') => Seq(3)
          case _ => Seq(4)
        }
      }
    }

    val ndfa2 = new NonDeterministicFiniteAutomata[Char, Int] {
      override val alphabet: Seq[Char] = CharAlphabets.Alphanumeric.toSeq
      override val initialStates: Seq[Int] = Seq(5)
      override val states: Seq[Int] = Seq(6, 7, 8, 9)
      override val acceptStates: Seq[Int] = Seq(8)

      override def transition[S >: Int](state: S, symbol: Char): Seq[Int] = {
        (state, symbol) match {
          case (5, 'd') => Seq(6)
          case (6, 'e') => Seq(7)
          case (7, 'f') => Seq(8)
          case _ => Seq(9)
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

    "or automata into (abc*|def)" in {
      val newNDFA = NonDeterministicFiniteAutomata.or(ndfa, ndfa2)
      assert(NonDeterministicFiniteAutomata.accepts(newNDFA, "abc"))
      assert(NonDeterministicFiniteAutomata.accepts(newNDFA, "abcc"))
      assert(NonDeterministicFiniteAutomata.accepts(newNDFA, "abccc"))
      assert(!NonDeterministicFiniteAutomata.accepts(newNDFA, "abbccc"))
      assert(NonDeterministicFiniteAutomata.accepts(newNDFA, "def"))
      assert(!NonDeterministicFiniteAutomata.accepts(newNDFA, "deff"))
    }

    "repeat automata into (abc*)*" in {
      val newNDFA = NonDeterministicFiniteAutomata.repeat(ndfa)

      assert(NonDeterministicFiniteAutomata.accepts(newNDFA, ""))
      assert(NonDeterministicFiniteAutomata.accepts(newNDFA, "abc"))
      assert(NonDeterministicFiniteAutomata.accepts(newNDFA, "abcc"))
      assert(NonDeterministicFiniteAutomata.accepts(newNDFA, "abccc"))
      assert(NonDeterministicFiniteAutomata.accepts(newNDFA, "abcabc"))
      assert(NonDeterministicFiniteAutomata.accepts(newNDFA, "abcabcc"))
      assert(NonDeterministicFiniteAutomata.accepts(newNDFA, "abccccabccc"))

      assert(!NonDeterministicFiniteAutomata.accepts(newNDFA, "abcab"))
      assert(!NonDeterministicFiniteAutomata.accepts(newNDFA, "abbccc"))
      assert(!NonDeterministicFiniteAutomata.accepts(newNDFA, "def"))
      assert(!NonDeterministicFiniteAutomata.accepts(newNDFA, "deff"))
    }

    "fromSeq" in {
      val ndfa = NonDeterministicFiniteAutomata.fromSeq("abcd")

      assert(!NonDeterministicFiniteAutomata.accepts(ndfa, ""))
      assert(!NonDeterministicFiniteAutomata.accepts(ndfa, "abc"))
      assert(!NonDeterministicFiniteAutomata.accepts(ndfa, "abcc"))
      assert(NonDeterministicFiniteAutomata.accepts(ndfa, "abcd"))

    }
  }
}
