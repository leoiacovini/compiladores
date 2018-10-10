package automata

import automata.ndpa.{NDPARunner, NonDeterministicPushdownAutomata}
import org.scalatest.WordSpec

class NonDeterministicPushdownAutomataTest extends WordSpec {

  "NDPA" must {
    "work" in {
      sealed trait PDAState
      case object Initial extends PDAState
      case object State1 extends PDAState
      case object O1 extends PDAState
      case object O2 extends PDAState
      case object End extends PDAState
      case object Error extends PDAState
      val ndpa = new NonDeterministicPushdownAutomata[Char, Char, PDAState] {
        override val inputAlphabet: Seq[Char] = CharAlphabets.Alphanumeric.toSeq
        override val stackAlphabet: Seq[Char] = CharAlphabets.Alphanumeric.toSeq
        override val initialStackSymbol: Char = '$'
        override val initialState: PDAState= Initial
        override val states: Seq[PDAState] = Seq(Initial, State1, O1, O2, End)
        override val acceptStates: Seq[PDAState] = Seq(End)
        override val trapState: PDAState = Error

        override def transition[S >: PDAState](state: S, inputSymbolOpt: Option[Char], stackSymbolOpt: Option[Char]): Seq[(S, Seq[Char])] = {
          (state, inputSymbolOpt, stackSymbolOpt) match {
            case (Initial, Some('a'), Some(stackSymbol)) => Seq((Initial, Seq('a', stackSymbol)))
            case (Initial, Some('b'), Some(stackSymbol)) => Seq((Initial, Seq('b', stackSymbol)))
            case (Initial, None, None) => Seq((State1, Seq.empty))
            case (State1, Some('a'), Some('a')) => Seq((State1, Seq.empty))
            case (State1, Some('b'), Some('b')) => Seq((State1, Seq.empty))
            case (State1, None, Some('$')) => Seq((End, Seq.empty))
          }
        }
      }

      val runner = NDPARunner.fromNDPA(ndpa)

      assert(runner.accepts("abba"))
      assert(runner.accepts("abbaabba"))
      assert(runner.accepts("bbaabb"))
      assert(runner.accepts("aaaa"))

      assert(runner.rejects("ab"))
      assert(runner.rejects("baaa"))
      assert(runner.rejects("aaaaab"))

    }

    "follow empty transitions" in {
      sealed trait PDAState
      case object Initial extends PDAState
      case object State1 extends PDAState
      case object State2 extends PDAState
      case object State3 extends PDAState
      case object State4 extends PDAState
      case object End extends PDAState
      case object Error extends PDAState

      val ndpa = new NonDeterministicPushdownAutomata[Char, Char, PDAState] {
        override val inputAlphabet: Seq[Char] = CharAlphabets.Alphanumeric.toSeq
        override val stackAlphabet: Seq[Char] = CharAlphabets.Alphanumeric.toSeq
        override val initialStackSymbol: Char = '$'
        override val initialState: PDAState= Initial
        override val states: Seq[PDAState] = Seq(Initial, State1, State2, State3, End)
        override val acceptStates: Seq[PDAState] = Seq(End)
        override val trapState: PDAState = Error

        override def transition[S >: PDAState](state: S, inputSymbolOpt: Option[Char], stackSymbolOpt: Option[Char]): Seq[(S, Seq[Char])] = {
          (state, inputSymbolOpt, stackSymbolOpt) match {
            case (Initial, Some('a'), Some(stackSymbol)) => Seq((State1, Seq('a', stackSymbol)))
            case (State1, None, None) => Seq((State2, Seq.empty))
            case (State2, None, None) => Seq((State3, Seq.empty))
            case (State3, None, Some('a')) => Seq((State4, Seq.empty))
            case (State4, None, Some('$')) => Seq((End, Seq.empty))
          }
        }
      }


      val runner = NDPARunner.fromNDPA(ndpa)

      assert(runner.accepts("a"))
    }

    "concat, or and kleene" in {
      sealed trait PDAState
      case object Initial extends PDAState
      case object Initial2 extends PDAState
      case object State1 extends PDAState
      case object State2 extends PDAState
      case object End extends PDAState
      case object End2 extends PDAState
      case object Error extends PDAState
      case object Error2 extends PDAState

      val ndpa = new NonDeterministicPushdownAutomata[Char, Char, PDAState] {
        override val inputAlphabet: Seq[Char] = CharAlphabets.Alphanumeric.toSeq
        override val stackAlphabet: Seq[Char] = CharAlphabets.Alphanumeric.toSeq
        override val initialStackSymbol: Char = '$'
        override val initialState: PDAState= Initial
        override val states: Seq[PDAState] = Seq(Initial, State1, State2, End)
        override val acceptStates: Seq[PDAState] = Seq(End)
        override val trapState: PDAState = Error

        override def transition[S >: PDAState](state: S, inputSymbolOpt: Option[Char], stackSymbolOpt: Option[Char]): Seq[(S, Seq[Char])] = {
          (state, inputSymbolOpt, stackSymbolOpt) match {
            case (Initial, Some('a'), Some(stackSymbol)) => Seq((Initial, Seq('a', stackSymbol)))
            case (Initial, Some('b'), Some('a')) => Seq((State1, Seq.empty))
            case (State1, Some('b'), Some('a')) => Seq((State1, Seq.empty))
            case (State1, None, Some('$')) => Seq((End, Seq.empty))
          }
        }
      }

      val ndpa2 = new NonDeterministicPushdownAutomata[Char, Char, PDAState] {
        override val inputAlphabet: Seq[Char] = CharAlphabets.Alphanumeric.toSeq
        override val stackAlphabet: Seq[Char] = CharAlphabets.Alphanumeric.toSeq
        override val initialStackSymbol: Char = '$'
        override val initialState: PDAState= Initial2
        override val states: Seq[PDAState] = Seq(Initial2, End2)
        override val acceptStates: Seq[PDAState] = Seq(End2)
        override val trapState: PDAState = Error2

        override def transition[S >: PDAState](state: S, inputSymbolOpt: Option[Char], stackSymbolOpt: Option[Char]): Seq[(S, Seq[Char])] = {
          (state, inputSymbolOpt, stackSymbolOpt) match {
            case (Initial2, Some('c'), Some('$')) => Seq((End2, Seq.empty))
            case (_, Some(_), Some(stackSymbol)) => Seq((Error2, Seq(stackSymbol)))
            case (_, Some(_), None) => Seq((Error2, Seq.empty))
          }
        }
      }


      val runner1 = NDPARunner.fromNDPA(ndpa)
      val runner2 = NDPARunner.fromNDPA(ndpa2)

      assert(runner1.accepts("ab"))
      assert(runner1.accepts("aabb"))
      assert(runner1.rejects("aab"))

      assert(runner2.accepts("c"))
      assert(runner2.rejects("cc"))

      val ndpa3 = NonDeterministicPushdownAutomata.concat(ndpa, ndpa2)
      val runner3 = NDPARunner.fromNDPA(ndpa3)

      assert(runner3.accepts("abc"))
      assert(runner3.accepts("aabbc"))
      assert(runner3.rejects("aabc"))
      assert(runner3.rejects("ab"))


      val ndpa4 = NonDeterministicPushdownAutomata.or(ndpa, ndpa2)
      val runner4 = NDPARunner.fromNDPA(ndpa4)


      assert(runner4.accepts("ab"))
      assert(runner4.accepts("aabb"))
      assert(runner4.rejects("aab"))

      assert(runner4.accepts("c"))
      assert(runner4.rejects("cc"))


      val ndpa5 = NonDeterministicPushdownAutomata.kleene(ndpa2)
      val runner5 = NDPARunner.fromNDPA(ndpa5)

      assert(runner5.accepts("c"))
      assert(runner5.accepts("cc"))
      assert(runner5.accepts("ccc"))

      assert(runner5.rejects("cb"))
      assert(runner5.rejects("cccb"))
    }
  }
}
