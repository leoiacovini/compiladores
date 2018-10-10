package automata

import automata.ndfa.{NDFAStack, NonDeterministicFiniteAutomata}
import org.scalatest.WordSpec

class NDFAStackTest extends WordSpec {
  "NDFAStack" must {
    "do something nice" in {
      sealed trait MachineIdExample
      case object FooMachine extends MachineIdExample
      case object ParMachine extends MachineIdExample

      val ndfa1 = new NonDeterministicFiniteAutomata[Char, Int] {
        override val alphabet: Seq[Char] = CharAlphabets.Alphanumeric.toSeq
        override val initialStates: Seq[Int] = Seq(0)
        override val states: Seq[Int] = Seq(0, 1, 2, 3)
        override val acceptStates: Seq[Int] = Seq(3)

        override def transition[S >: Int](state: S, symbol: Char): Seq[Int] = {
          (state, symbol) match {
            case (0, 'f') => Seq(1)
            case (1, 'o') => Seq(2)
            case (2, 'o') => Seq(3)
            case _ => Seq.empty
          }
        }
      }

      val ndfa2 = new NonDeterministicFiniteAutomata[Char, Int] {
        override val alphabet: Seq[Char] = CharAlphabets.Alphanumeric.toSeq
        override val initialStates: Seq[Int] = Seq(0)
        override val states: Seq[Int] = Seq(0, 1, 2)
        override val acceptStates: Seq[Int] = Seq(2)

        override def transition[S >: Int](state: S, symbol: Char): Seq[Int] = {
          (state, symbol) match {
            case (1, ')') => Seq(2)
            case _ => Seq.empty
          }
        }
      }

      val machinesMap: Map[MachineIdExample, NonDeterministicFiniteAutomata[Char, Int]] =
        Map(
          FooMachine -> ndfa1,
          ParMachine -> ndfa2
        )

      val submachinesCalls: PartialFunction[(MachineIdExample, Int, Char), (MachineIdExample, Int)] = {
        case (FooMachine, 0, '(') => (FooMachine, 0)
      }

      val stack = NDFAStack(machinesMap, submachinesCalls, Seq.empty)

//      assert(NDFAStack.accepts(stack.start(FooMachine), "foo"))
      assert(NDFAStack.accepts(stack.start(FooMachine), "(foo)"))
      assert(NDFAStack.accepts(stack.start(FooMachine), "((foo))"))
      assert(NDFAStack.accepts(stack.start(FooMachine), "(((foo)))"))

      assert(!NDFAStack.accepts(stack.start(FooMachine), "(((foo)}}"))

      assert(!NDFAStack.accepts(stack.start(FooMachine), "(((fo))"))
      assert(!NDFAStack.accepts(stack.start(FooMachine), "(((foo))"))
      assert(!NDFAStack.accepts(stack.start(FooMachine), "(foo))"))
      assert(!NDFAStack.accepts(stack.start(FooMachine), "(fsod"))
    }
  }

}
