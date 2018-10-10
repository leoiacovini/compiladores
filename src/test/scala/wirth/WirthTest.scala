package wirth

import automata.ndpa.NDPARunner
import org.scalatest.WordSpec

class WirthTest extends WordSpec {
  "WirthExpression" must {
    "work" in {
      val expression = WirthExpression()

      val runner = NDPARunner.fromNDPA(expression)

      assert(runner.accepts(Terminal("foo") :: Nil))

      assert(runner.accepts(NonTerminal("foo") :: Nil))

      assert(runner.accepts(Terminal("foo") :: Terminal("bar") :: Terminal("baz") :: Nil))
      assert(runner.accepts(Seq(Terminal("foo"), NonTerminal("bar"), Terminal("baz"))))

      assert(runner.accepts(Seq(Terminal("foo"), Special("|"), Terminal("baz"))))
      assert(runner.accepts(Seq(Terminal("foo"), Special("("), Terminal("baz"), Special(")"))))
      assert(runner.accepts(Seq(Terminal("foo"), Special("{"), Terminal("baz"), Special("}"))))
      assert(runner.accepts(Seq(Terminal("foo"), Special("["), Terminal("baz"), Special("]"))))
      assert(runner.accepts(Seq(Terminal("foo"), Special("("), Terminal("baz"), Special("|"), NonTerminal("foo"), Special(")"))))
      assert(runner.accepts(Seq(Terminal("foo"), Special("|"), Terminal("baz"))))

      assert(runner.rejects(Seq(Terminal("foo"), Special("|"))))
      assert(runner.rejects(Seq(Terminal("foo"), Special("{"), Terminal("baz"), Special("]"))))
      assert(runner.rejects(Seq(Terminal("foo"), Special("("), Terminal("baz"))))
      assert(runner.rejects(Seq(Terminal("foo"), Special("("), Terminal("baz"), Special("|"), Special(")"))))
    }
  }

  "WirthGrammar" must {
    "work" in {
      val grammar = WirthGrammar()

      val runner = NDPARunner.fromNDPA(grammar)

      assert(runner.accepts(NonTerminal("a") :: Special("=") :: Terminal("foo") :: Special(".") :: Nil))
      assert(runner.accepts(NonTerminal("a") :: Special("=") :: Terminal("foo") :: Terminal("foo") :: Special(".") :: Nil))
      assert(runner.accepts(NonTerminal("a") :: Special("=") :: Terminal("foo") :: Special("|") :: Terminal("foo") :: Special(".") :: Nil))


      assert(runner.rejects(NonTerminal("a") :: Special("=") :: Terminal("foo") :: Special("|") :: Special(".") :: Nil))

    }
  }
}
