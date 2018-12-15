package common.automata

import common.automata.dpa.{DPARunner, DeterministicPushdownAutomata}
import org.scalatest.WordSpec

class DeterministicPushdownAutomataTest extends WordSpec {

  "DPA" must {
    "work" in {
      sealed trait PDAState
      case object Initial extends PDAState
      case object F extends PDAState
      case object O1 extends PDAState
      case object O2 extends PDAState
      case object End extends PDAState
      case object Error extends PDAState
      val fooDPA = new DeterministicPushdownAutomata[Char, Char, PDAState] {
        override val inputAlphabet: Seq[Char] = CharAlphabets.Alphanumeric.toSeq
        override val stackAlphabet: Seq[Char] = CharAlphabets.Alphanumeric.toSeq
        override val initialStackSymbol: Char = '$'
        override val initialState: PDAState= Initial
        override val states: Seq[PDAState] = Seq(Initial, F, O1, O2, End)
        override val acceptStates: Seq[PDAState] = Seq(End)
        override val trapState: PDAState = Error

        override def transition[S >: PDAState](state: S, inputSymbolOpt: Option[Char], stackSymbol: Char): (S, Seq[Char]) = {
          (state, inputSymbolOpt, stackSymbol) match {
            case (Initial, Some('('), '$') => (Initial, "($")
            case (Initial, Some('('), '(') => (Initial, "((")
            case (Initial, Some('f'), _) => (F, Seq(stackSymbol))
            case (F, Some('o'), _) => (O1, Seq(stackSymbol))
            case (O1, Some('o'), _) => (O2, Seq(stackSymbol))
            case (O2, Some(')'), '(') => (O2, "")
            case (O2, None, '$') => (End, "")
            case (_, None, _) => (state, Seq(stackSymbol))
            case _ => (Error, Seq(stackSymbol))
          }
        }
      }

      val runner = DPARunner.fromDPA(fooDPA)
      assert(runner.accepts("foo"))
      assert(runner.accepts("(foo)"))
      assert(runner.accepts("((foo))"))
      assert(runner.accepts("(((foo)))"))

      assert(runner.rejects("{{foo}}"))
      assert(runner.rejects("{(foo)}"))
      assert(runner.rejects("((foo)}"))

      assert(runner.rejects("fod"))
      assert(runner.rejects("(foo"))
      assert(runner.rejects("(foo))"))
      assert(runner.rejects("(((fo)))"))
    }

    "exp" in {

      sealed trait ExpressionAlphabet

      case class Terminal(name: String) extends ExpressionAlphabet
      case class NonTerminal(name: String) extends ExpressionAlphabet
      case class Special(str: String) extends ExpressionAlphabet
      case object EmptyString extends ExpressionAlphabet

      sealed trait PDAState
      case object Initial extends PDAState
      case object ExpressionRecognized extends PDAState
      case object Error extends PDAState
      val expDPA = new DeterministicPushdownAutomata[ExpressionAlphabet, Char, PDAState] {
        override val inputAlphabet: Seq[ExpressionAlphabet] = Seq(Terminal("any"), NonTerminal("any"))
        override val stackAlphabet: Seq[Char] = CharAlphabets.Alphanumeric.toSeq
        override val initialStackSymbol: Char = '$'
        override val initialState: PDAState= Initial
        override val states: Seq[PDAState] = Seq(Initial, ExpressionRecognized)
        override val acceptStates: Seq[PDAState] = Seq(ExpressionRecognized)
        override val trapState: PDAState = Error

        override def transition[S >: PDAState](state: S, inputSymbolOpt: Option[ExpressionAlphabet], stackSymbol: Char): (S, Seq[Char]) = {
          val stackNotChanged = Seq(stackSymbol)
          val openCloseContextMap = Map[String, String](
            "(" -> ")",
            "[" -> "]",
            "{" -> "}"
          )
          (state, inputSymbolOpt, stackSymbol) match {
            case (Initial, Some(Terminal(_)), _) => (ExpressionRecognized, stackNotChanged)
            case (Initial, Some(NonTerminal(_)), _) => (ExpressionRecognized, stackNotChanged)
            case (Initial, Some(EmptyString), _) => (ExpressionRecognized, stackNotChanged)
            case (Initial, Some(Special(open)), _) if openCloseContextMap.keys.toSeq.contains(open) => (Initial, Seq(open.charAt(0), stackSymbol))
            case (ExpressionRecognized, Some(Special(close)), openchar) if openCloseContextMap.map(_.swap).get(close).contains(openchar.toString) => (ExpressionRecognized, Seq.empty)
            case (ExpressionRecognized, Some(Special(open)), _) if openCloseContextMap.keys.toSeq.contains(open) => (ExpressionRecognized, Seq(open.charAt(0), stackSymbol))
            case (ExpressionRecognized, Some(Special("|")), _) => (Initial, stackNotChanged)
            case (ExpressionRecognized, Some(Terminal(_)), _) => (ExpressionRecognized, stackNotChanged)
            case (ExpressionRecognized, Some(NonTerminal(_)), _) => (ExpressionRecognized, stackNotChanged)
            case (ExpressionRecognized, Some(EmptyString), _) => (ExpressionRecognized, stackNotChanged)
//            case (ExpressionRecognized, None, '$') => (ExpressionRecognized, Seq.empty)
            case (_, None, _) => (state, stackNotChanged)
            case _ => (Error, stackNotChanged)
          }
        }
      }

      val runner = DPARunner.fromDPA(expDPA)
      assert(runner.accepts(Seq(Terminal("foo"))))
      assert(runner.accepts(Seq(NonTerminal("foo"))))
      assert(runner.accepts(Seq(EmptyString)))
//
      assert(runner.accepts(Seq(Terminal("foo"), Terminal("bar"), Terminal("baz"))))
      assert(runner.accepts(Seq(Terminal("foo"), NonTerminal("bar"), Terminal("baz"))))
      assert(runner.accepts(Seq(Terminal("foo"), Special("("), Terminal("baz"), Special(")"))))
      assert(runner.accepts(Seq(Terminal("foo"), Special("{"), Terminal("baz"), Special("}"))))
      assert(runner.accepts(Seq(Terminal("foo"), Special("["), Terminal("baz"), Special("]"))))
      assert(runner.accepts(Seq(Terminal("foo"), Special("("), Terminal("baz"), Special("|"), NonTerminal("foo"), Special(")"))))
//
//
      assert(runner.accepts(Seq(Terminal("foo"), Special("|"), Terminal("baz"))))
//
      assert(runner.rejects(Seq(Terminal("foo"), Special("|"))))
      assert(runner.rejects(Seq(Terminal("foo"), Special("{"), Terminal("baz"), Special("]"))))
      assert(runner.rejects(Seq(Terminal("foo"), Special("("), Terminal("baz"))))
      assert(runner.rejects(Seq(Terminal("foo"), Special("("), Terminal("baz"), Special("|"), Special(")"))))
//

    }


  }

}
