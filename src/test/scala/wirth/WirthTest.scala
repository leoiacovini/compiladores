package wirth

import automata.ndpa.NDPARunner
import org.scalatest.WordSpec
import wirth.WirthExperimentation.NextExpression
import wirth.WirthNotation.{ExpressionInitial, ExpressionRecognized, WirthState}
import wirth.WirthToNDPA.ExpressionContext

class WirthTest extends WordSpec {
  val wirthRules = Map(
    NonTerminalToken("PROGRAM") ->
      Sequence(
        NonTerminalToken("ASSIGNMENT"),
        ExpressionKleene(
          Sequence(
            TerminalToken(";"),
            NonTerminalToken("ASSIGNMENT")
          )
        )
      ),
    NonTerminalToken("ASSIGNMENT") ->
      Sequence(
        TerminalToken("LET"),
        TerminalToken("id"),
        TerminalToken("="),
        NonTerminalToken("EXPRESSION")
      ),
    NonTerminalToken("EXPRESSION") ->
      Sequence(
        NonTerminalToken("TERM"),
        ExpressionKleene(
          Sequence(
            Or(
              TerminalToken("+"),
              TerminalToken("-")
            ),
            NonTerminalToken("TERM")
          )
        )
      ),
    NonTerminalToken("TERM") ->
      Sequence(
        NonTerminalToken("FACTOR"),
        ExpressionKleene(
          Sequence(
            Or(
              TerminalToken("*"),
              TerminalToken("/")
            ),
            NonTerminalToken("FACTOR")
          )
        )
      ),
    NonTerminalToken("FACTOR") ->
      Or(
        TerminalToken("id"),
        TerminalToken("num"),
        Sequence(
          TerminalToken("("),
          NonTerminalToken("EXPRESSION"),
          TerminalToken(")"),
        )
      )
  )

  val dependencyMap: Map[NonTerminalToken, Seq[NonTerminalToken]] = Map(
    NonTerminalToken("PROGRAM") -> Seq(
      NonTerminalToken("ASSIGNMENT")
    ),
    NonTerminalToken("ASSIGNMENT") -> Seq(
      NonTerminalToken("EXPRESSION")
    ),
    NonTerminalToken("EXPRESSION") -> Seq(
      NonTerminalToken("TERM")
    ),
    NonTerminalToken("TERM") -> Seq(
      NonTerminalToken("FACTOR")
    ),
    NonTerminalToken("FACTOR") -> Seq(
      NonTerminalToken("EXPRESSION")
    )
  )

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

    "foo" in {
      val x1 = Seq[(WirthState, Option[WirthStuff], Option[Char])](
        (ExpressionInitial, Some(Special("(")), Some('$')),
          (ExpressionInitial, Some(NonTerminal("foo")), Some('(')),
          (ExpressionRecognized, Some(Special("|")), Some('(')),
          (ExpressionInitial, Some(NonTerminal("bar")), Some('(')),
          (ExpressionRecognized, Some(Special(")")), Some('(')),
          (ExpressionRecognized, None, Some('$'))
      )

      val x2 = Seq(Special("("), NonTerminal("foo"), Special("|"), NonTerminal("bar"), Special(")"))
      val x3 = Seq(Special("("), NonTerminal("foo"), Special("|"), NonTerminal("bar"), Special(")"), Special("("), Terminal("baz"), Special(")"))
      val x4 = Seq(Special("("), NonTerminal("foo"), Special("|"), NonTerminal("bar"), Special(")"), Special("{"), Terminal("baz"), Special("}"))
      val x5 = Seq(Special("{"), NonTerminal("foo"), Special("|"), NonTerminal("bar"), Special("}"), Special("("), Terminal("baz"), Special(")"))
      //      val NextExpression(next, rest) = WirthExperimentation.getNextExpression(x6)
      val x6 = Seq(NonTerminal("foo"), NonTerminal("foo2"), Special("("), Terminal("bar1"), Special(")"), Special("|"), NonTerminal("bar2"), Special("{"), Terminal("baz"), Special("}"))

      val parsedExpects = Or(
        Sequence(
          NonTerminalToken(NonTerminal("foo")),
          NonTerminalToken(NonTerminal("foo2")),
          ExpressionParentesis(
            TerminalToken(Terminal("bar1"))
          )
        ),
        Sequence(
          NonTerminalToken(NonTerminal("bar2")),
          ExpressionKleene(
            TerminalToken(Terminal("baz"))
          )
        )
      )

      val parsed = WirthExperimentation.parse(x6)
      assert(parsed === parsedExpects)
      //      println(next)
//      println(rest)
//
//      val NextExpression(next2, rest2) = WirthExperimentation.getNextExpression(rest)
//      println(next2)
//      println(rest2)
//
//      val NextExpression(next3, rest3) = WirthExperimentation.getNextExpression(rest2)
//      println(next3)
//      println(rest3)
//
//      val NextExpression(next4, rest4) = WirthExperimentation.getNextExpression(rest3)
//      println(next4)
//      println(rest4)

    }

    "bar" in {
      val grammar = Map(
        NonTerminalToken(NonTerminal("A")) ->
            Sequence(
              TerminalToken(Terminal("a")),
              NonTerminalToken(NonTerminal("B")),
              TerminalToken(Terminal("b"))
            ),
        NonTerminalToken(NonTerminal("B")) ->
            Or(
              TerminalToken(Terminal("c")),
              NonTerminalToken(NonTerminal("A"))
            )
      )
      WirthExperimentation.printRules {
        grammar
      }

      WirthExperimentation.printRules {
        WirthExperimentation.reduceRules(grammar)
      }
    }
  }

  "NDPA from Wirth" must {
    "sequence of terminals" in {
      val seqOfTerminals = Sequence(
        TerminalToken(Terminal("a")),
        TerminalToken(Terminal("b")),
        TerminalToken(Terminal("c")),
      )

      val wirthNDPA = WirthToNDPA.fromSequenceOfTerminals(seqOfTerminals)

      val runner = NDPARunner.fromNDPA(wirthNDPA)

      val seq = Seq(
        Terminal("a"),
        Terminal("b"),
        Terminal("c"),
      )
      assert(runner.accepts(seq))
      assert(runner.rejects(seq :+ Terminal("a")))
      assert(runner.rejects(Terminal("a") +: seq))
      assert(runner.rejects(seq.drop(1)))
      assert(runner.rejects(seq.dropRight(1)))
//      println(runner.runAll(seq))
    }

    "from expression" in {
      val seqOfTerminals1 = Sequence(
        TerminalToken(Terminal("a")),
        TerminalToken(Terminal("b")),
        TerminalToken(Terminal("c")),
      )

      val seqOfTerminals2 = Sequence(
        TerminalToken(Terminal("d")),
        TerminalToken(Terminal("e")),
        TerminalToken(Terminal("f")),
      )

      val or = Or(seqOfTerminals1, seqOfTerminals2)
      val root = NonTerminalToken("ROOT")
      val context = ExpressionContext(Seq(root), Map(root -> or))
      val wirthNDPA = WirthToNDPA.fromExpression(or, context)

      val runner = NDPARunner.fromNDPA(wirthNDPA)
      val seq1 = Seq(
        Terminal("a"),
        Terminal("b"),
        Terminal("c"),
      )

      val seq2 = Seq(
        Terminal("d"),
        Terminal("e"),
        Terminal("f"),
      )
      assert(runner.accepts(seq1))
      assert(runner.accepts(seq2))

      assert(runner.rejects(seq1 :+ Terminal("a")))
      assert(runner.rejects(Terminal("a") +: seq1))
      assert(runner.rejects(seq1.drop(1)))
      assert(runner.rejects(seq1.dropRight(1)))

      assert(runner.rejects(seq2 :+ Terminal("a")))
      assert(runner.rejects(Terminal("a") +: seq2))
      assert(runner.rejects(seq2.drop(1)))
      assert(runner.rejects(seq2.dropRight(1)))

      assert(runner.rejects(seq1 ++ seq2))
      assert(runner.rejects(seq2 ++ seq1))

    }

    "from rules" in {
      val root = NonTerminalToken("ROOT")

      // root -> root "a" | "b"
      val leftRecursion = Or(
        Sequence(
          root,
          TerminalToken("a")
        ),
        TerminalToken("b")
      )
      val rules = Map(root -> leftRecursion)
      val wirthNDPA = WirthToNDPA.fromRules(rules, root)

      val runner = NDPARunner.fromNDPA(wirthNDPA)

      assert(runner.accepts(Seq(Terminal("b"), Terminal("a"))))
      assert(runner.accepts(Seq(Terminal("b"), Terminal("a"), Terminal("a"))))
      assert(runner.accepts(Seq(Terminal("b"), Terminal("a"), Terminal("a"), Terminal("a"))))

      assert(runner.rejects(Seq(Terminal("a"), Terminal("a"), Terminal("b"), Terminal("b"))))
      assert(runner.rejects(Seq(Terminal("a"), Terminal("a"), Terminal("a"), Terminal("b"))))
      assert(runner.rejects(Seq(Terminal("a"), Terminal("a"), Terminal("a"), Terminal("a"))))



      // root -> "a" root | "b"
      val rightRecursion = Or(
        Sequence(
          TerminalToken("a"),
          root
        ),
        TerminalToken("b")
      )
      val rules2 = Map(root -> rightRecursion)
      val wirthNDPA2 = WirthToNDPA.fromRules(rules2, root)

      val runner2 = NDPARunner.fromNDPA(wirthNDPA2)

      assert(runner2.accepts(Seq(Terminal("a"), Terminal("b"))))
      assert(runner2.accepts(Seq(Terminal("a"), Terminal("a"), Terminal("b"))))
      assert(runner2.accepts(Seq(Terminal("a"), Terminal("a"), Terminal("a"), Terminal("b"))))

      assert(runner2.rejects(Seq(Terminal("a"), Terminal("a"), Terminal("b"), Terminal("b"))))
      assert(runner2.rejects(Seq(Terminal("b"), Terminal("a"), Terminal("a"), Terminal("a"))))
      assert(runner2.rejects(Seq(Terminal("a"), Terminal("a"), Terminal("a"), Terminal("a"))))
    }
  }
}
