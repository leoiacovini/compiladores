package wirth

import automata.ndpa.{NDPARunner, NonDeterministicPushdownAutomata}
import org.scalatest.WordSpec
import wirth.WirthExperimentation.NextExpression
import wirth.WirthNotation.{ExpressionInitial, ExpressionRecognized, WirthState}
import wirth.WirthToNDPA._

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

    "from rules auto recursion" in {
      val root = NonTerminalToken("ROOT")

      // root -> "(" root ")" | "a"
      val autoRecursion = Or(
        Sequence(
          TerminalToken("("),
          root,
          TerminalToken(")")
        ),
        TerminalToken("a")
      )
      val rules = Map(root -> autoRecursion)
      val wirthNDPA: NonDeterministicPushdownAutomata[WirthStuff, StackAlphabet, WirthGeneratedState] = WirthToNDPA.fromRules(rules, root)
      val inputAlphabet: Seq[Option[WirthStuff]] = Seq(None, Some(Terminal("(")), Some(Terminal("a")), Some(Terminal(")")))
      val stackAlphabet: Seq[Option[StackAlphabet]] = Seq(None, Some(InitialStackSymbol), Some(CentralAutoRecursion("(")))
      val tries = for { input <- inputAlphabet
                        stack <- stackAlphabet
                        state <- wirthNDPA.states } yield (state, input, stack)
      val runner = NDPARunner.fromNDPA(wirthNDPA)
      NonDeterministicPushdownAutomata.printTransitions(wirthNDPA, tries)
//      assert(runner.accepts(Seq(Terminal("a"))))
//      println(runner.debugString)
//      println(runner.runAll(Seq(Terminal("("))).debugString)
//      println(runner.runAll(Seq(Terminal("("), Terminal("a"))).debugString)
//      println(runner.runAll(Seq(Terminal("("), Terminal("a"), Terminal(")"))).debugString)
//      assert(runner.accepts(Seq(Terminal("("), Terminal("a"), Terminal(")"))))

      Seq(
        (RuleInitialState(root), None, None)
          -> (PartialSequence(Sequence(TerminalToken("(")), 0), Seq.empty),
        (PartialSequence(Sequence(TerminalToken("(")), 0), None, Some(InitialStackSymbol))
          -> (AcceptSequence(Sequence(TerminalToken("("))), Seq(InitialStackSymbol)),
        (AcceptSequence(Sequence(TerminalToken("("))), None, None)
          -> (RuleInitialState(root), Seq(CentralAutoRecursion("("))),
        (RuleInitialState(root), None, None)
          -> (PartialSequence(Sequence(TerminalToken("(")), 0), Seq.empty),
        (PartialSequence(Sequence(TerminalToken("(")), 0), None, None)
          -> (PartialSequence(Sequence(TerminalToken("a")), 0), Seq.empty),
        (PartialSequence(Sequence(TerminalToken("a")), 0), Some(Terminal("a")), None)
          -> (AcceptSequence(Sequence(TerminalToken("a"))), Seq.empty),
        (AcceptSequence(Sequence(TerminalToken("a"))), None, None)
          -> (PartialSequence(Sequence(TerminalToken(")")), 0), Seq.empty),
        (PartialSequence(Sequence(TerminalToken(")")), 0), Some(Terminal(")")), None)
          -> (AcceptSequence(Sequence(TerminalToken(")"))), Seq.empty),
        (AcceptSequence(Sequence(TerminalToken(")"))), None, Some(CentralAutoRecursion("(")))
          -> (AcceptSequence(Sequence(TerminalToken(")"))), Seq.empty)
      )
//      assert(runner.accepts(Seq(Terminal("("), Terminal("("), Terminal("a"), Terminal(")"), Terminal(")"))))


//      assert(runner.rejects(Seq(Terminal("("), Terminal("a"))))
//      assert(runner.rejects(Seq(Terminal("("), Terminal("("), Terminal("a"), Terminal(")"))))
//      assert(runner.rejects(Seq(Terminal("("), Terminal("a"), Terminal(")"), Terminal(")"))))
    }
  }
  "Testing isRecursive stuff" in {
    val root = NonTerminalToken("ROOT")

    val autoRecursion = Or(
      Sequence(
        TerminalToken("("),
        root,
        TerminalToken(")")
      ),
      TerminalToken("a")
    )
    val rules = Map(root -> autoRecursion)
    val context = ExpressionContext(Seq.empty, rules)
    assert(context.isRecursiveWithoutContext(root))


    val rules2 = Map(
      root -> Sequence(NonTerminalToken("A")),
      NonTerminalToken("A") -> Sequence(NonTerminalToken("B")),
      NonTerminalToken("B") -> Sequence(NonTerminalToken("C")),
      NonTerminalToken("C") -> Sequence(NonTerminalToken("D")),
      NonTerminalToken("D") -> Sequence(TerminalToken("done"))
    )
    val context2 = ExpressionContext(Seq.empty, rules2)
    assert(!context2.isRecursiveWithoutContext(root))


    val rules3 = Map(
      root -> Sequence(NonTerminalToken("A")),
      NonTerminalToken("A") -> Sequence(NonTerminalToken("B")),
      NonTerminalToken("B") -> Sequence(NonTerminalToken("C")),
      NonTerminalToken("C") -> Sequence(NonTerminalToken("D")),
      NonTerminalToken("D") -> Sequence(NonTerminalToken("C"))
    )
    val context3 = ExpressionContext(Seq.empty, rules3)
    assert(!context3.isRecursiveWithoutContext(root))


    val rules4 = Map(
      root -> Sequence(NonTerminalToken("A")),
      NonTerminalToken("A") -> Sequence(NonTerminalToken("B")),
      NonTerminalToken("B") -> Sequence(NonTerminalToken("C")),
      NonTerminalToken("C") -> Sequence(NonTerminalToken("D")),
      NonTerminalToken("D") -> Sequence(root)
    )
    val context4 = ExpressionContext(Seq.empty, rules4)
    assert(context4.isRecursiveWithoutContext(root))

  }
  def space() = (1 to 4000).map(_ => "").foreach(println)
  "Central recursion apply" in {
    val seqOfTerminals = Sequence(
      TerminalToken(Terminal("a")),
      TerminalToken(Terminal("b")),
      TerminalToken(Terminal("c"))
    )

    val seqNdpa = WirthToNDPA.fromSequenceOfTerminals(seqOfTerminals)

    val centralRecursion = CentralRecursion[StackAlphabet](
      Map(
        CentralAutoRecursion("(") -> Sequence(TerminalToken("(")),
        CentralAutoRecursion("[") -> Sequence(TerminalToken("["))
      ),
      Map(
        CentralAutoRecursion("(") -> Sequence(TerminalToken(")")),
        CentralAutoRecursion("[") -> Sequence(TerminalToken("]"))
      )
    )
    val root = NonTerminalToken("ROOT")

    val autoRecursion = Or(
      Sequence(
        TerminalToken("("),
        root,
        TerminalToken(")")
      ),
      seqOfTerminals
    )
    val rules = Map(root -> autoRecursion)
    val context = ExpressionContext(Seq(root), rules)
    val ndpa = WirthToNDPA.applyCentralRecursion(seqNdpa, centralRecursion, context)
    val runnerSeq = NDPARunner.fromNDPA(seqNdpa)
    val runner = NDPARunner.fromNDPA(ndpa)

//    assert(runnerSeq.accepts(Seq(Terminal("a"), Terminal("b"), Terminal("c"))))
//    assert(runner.accepts(Seq(Terminal("a"), Terminal("b"), Terminal("c"))))

//    space()
//    val run1 = runner.runAll(Seq(Terminal("(")))
//    println("wat")
//    println(run1.debugString)
//    assert {
//      run1.current.exists {c =>
//        c.state == PartialSequence(Sequence(TerminalToken("a"), TerminalToken("b"), TerminalToken("c")), 0) &&
//          c.stack.headOption.contains(CentralAutoRecursion("("))
//      }
//    }

//    val run2 = runner.runAll(Seq(Terminal("("), Terminal("a")))
//    println("wat2")
//    println(run2.debugString)
//    assert {
//      run2.current.exists {c =>
//        c.state == PartialSequence(Sequence(TerminalToken("a"), TerminalToken("b"), TerminalToken("c")), 1) &&
//          c.stack.headOption.contains(CentralAutoRecursion("("))
//      }
//    }

//    val run3 = runner.runAll(Seq(Terminal("("), Terminal("a"), Terminal("b")))
//    println("wat3")
//    println(run3.debugString)
//    assert {
//      run3.current.exists {c =>
//        c.state == PartialSequence(Sequence(TerminalToken("a"), TerminalToken("b"), TerminalToken("c")), 2) &&
//          c.stack.headOption.contains(CentralAutoRecursion("("))
//      }
//    }

//    val run4_2 = run3.run(Some(Terminal("c")))
//    println("wat4_2")
//    println(run4_2.debugString)
//
//    val run4 = runner.runAll(Seq(Terminal("("), Terminal("a"), Terminal("b"), Terminal("c")))
//    println("wat4")
//    println(run4.debugString)
//    assert {
//      run4.current.exists {c =>
//      c.state == AcceptSequence(Sequence(TerminalToken("a"), TerminalToken("b"), TerminalToken("c"))) &&
//        c.stack.headOption.contains(CentralAutoRecursion("("))
//      }
//    }
//
//    assert {
//      run4.current.exists {c =>
//      c.state == PartialSequence(Sequence(TerminalToken(")")), 0) &&
//        c.stack.headOption.contains(CentralAutoRecursion("("))
//      }
//    }

    val inputAlphabet: Seq[Option[WirthStuff]] = Seq(None, Some(Terminal("(")), Some(Terminal("a")), Some(Terminal("b")), Some(Terminal("c")), Some(Terminal(")")))
    val stackAlphabet: Seq[Option[StackAlphabet]] = Seq(None, Some(InitialStackSymbol), Some(CentralAutoRecursion("(")))
    val tries = for { input <- inputAlphabet
                        stack <- stackAlphabet
                        state <- ndpa.states } yield (state, input, stack)
    NonDeterministicPushdownAutomata.printTransitions(ndpa, tries)

//    val run5_2 = run4.run(Some(Terminal(")")))
//    println("wat5_2")
//    println(run5_2.debugString)
//    val run5 = runner.runAll(Seq(Terminal("("), Terminal("a"), Terminal("b"), Terminal("c"), Terminal(")")))
//    println("wat5")
//    println(run5.debugString)
//    assert {
//      run5.current.exists {c =>
//        c.state == AcceptSequence(Sequence(TerminalToken("a"), TerminalToken("b"), TerminalToken("c"))) &&
//          c.stack.headOption.isEmpty
//      }
//    }

    assert(runner.accepts(Seq(Terminal("a"), Terminal("b"), Terminal("c"))))
    assert(runner.accepts(Seq(Terminal("("), Terminal("a"), Terminal("b"), Terminal("c"), Terminal(")"))))
    assert(runner.accepts(Seq(Terminal("("), Terminal("("), Terminal("a"), Terminal("b"), Terminal("c"), Terminal(")"), Terminal(")"))))
    assert(runner.accepts(Seq(Terminal("["), Terminal("a"), Terminal("b"), Terminal("c"), Terminal("]"))))
    assert(runner.accepts(Seq(Terminal("["), Terminal("("), Terminal("a"), Terminal("b"), Terminal("c"), Terminal(")"), Terminal("]"))))

    assert(runner.rejects(Seq(Terminal("a"), Terminal("b"))))
    assert(runner.rejects(Seq(Terminal("("), Terminal("a"), Terminal("b"), Terminal(")"))))
//    println {
//      "DEBUG\n" +
//      runner.runAll(Seq(Terminal("("))).debugString
//    }

//    assert(runner.rejects(Seq(Terminal("("))))
//    println {
//      "DEBUG\n" +
//      runner.runAll(Seq(Terminal("("), Terminal("a"), Terminal("b"), Terminal("c"))).debugString
//    }
    assert(runner.rejects(Seq(Terminal("("), Terminal("a"), Terminal("b"), Terminal("c"))))
    assert(runner.rejects(Seq(Terminal("a"), Terminal("b"), Terminal("c"), Terminal(")"))))
    assert(runner.rejects(Seq(Terminal("["), Terminal("("), Terminal("a"), Terminal("b"), Terminal("c"), Terminal("]"), Terminal(")"))))


  }
}
