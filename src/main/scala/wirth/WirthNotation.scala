package wirth

import common.automata.CharAlphabets
import common.automata.ndpa.{NDPARunner, NonDeterministicPushdownAutomata}
import jdk.nashorn.internal.parser.TokenStream
import wirth.WirthNotation._
import wirth.WirthToNDPA.{ExpressionContext, fromExpressionWithoutRecursion}
import java.util.UUID.randomUUID
import wirth.WirthLexer.GeneratedRunner
import scala.annotation.tailrec

trait WirthLexicalToken
case class Terminal(str: String) extends WirthLexicalToken
case class NonTerminal(str: String) extends WirthLexicalToken
case class Special(str: String) extends WirthLexicalToken
case object EmptySequence extends WirthLexicalToken
case object Arrow extends WirthLexicalToken

object WirthNotation {
  sealed trait WirthState
  case object GrammarInit extends WirthState
  case object ExpectingEquals extends WirthState
  case object RuleRecognized extends WirthState
  case object ExpressionInitial extends WirthState
  case object ExpressionRecognized extends WirthState
  case object ExpressionError extends WirthState

}

case class WirthExpression() extends NonDeterministicPushdownAutomata[WirthLexicalToken, Char, WirthState] {
  override val inputAlphabet: Seq[WirthLexicalToken] = Seq.empty
  override val stackAlphabet: Seq[Char] = CharAlphabets.Alphanumeric.toSeq
  override val initialStackSymbol: Char = '$'
  override val initialState: WirthState = ExpressionInitial
  override val states: Seq[WirthState] = Seq(ExpressionInitial, ExpressionRecognized, ExpressionError)
  override val acceptStates: Seq[WirthState] = Seq(ExpressionRecognized)
  override val trapState: WirthState = ExpressionError

  override def transition[S >: WirthState](state: S, inputSymbolOpt: Option[WirthLexicalToken], stackSymbolOpt: Option[Char]): Seq[(S, Seq[Char])] = {
    val notChanged = stackSymbolOpt.map(s => Seq(s)).getOrElse(Seq.empty)
    (state, inputSymbolOpt, stackSymbolOpt) match {
      case (ExpressionInitial, Some(Special("(")), Some(stackSymbol)) => (ExpressionInitial, Seq('(', stackSymbol)) :: Nil
      case (ExpressionInitial, Some(Special("{")), Some(stackSymbol)) => (ExpressionInitial, Seq('{', stackSymbol)) :: Nil
      case (ExpressionInitial, Some(Special("[")), Some(stackSymbol)) => (ExpressionInitial, Seq('[', stackSymbol)) :: Nil
      case (ExpressionInitial, Some(NonTerminal(nt)), Some(_)) => (ExpressionRecognized, notChanged) :: Nil
      case (ExpressionInitial, Some(Terminal(t)), Some(_)) => (ExpressionRecognized, notChanged) :: Nil
      case (ExpressionInitial, Some(EmptySequence), Some(_)) => (ExpressionRecognized, notChanged) :: Nil
      case (ExpressionRecognized, None, None) => (ExpressionInitial, notChanged) :: Nil
      case (ExpressionRecognized, None, Some('$')) => (ExpressionRecognized, Seq.empty) :: Nil
      case (ExpressionRecognized, Some(Special("|")), _) => (ExpressionInitial, notChanged) :: Nil
      case (ExpressionRecognized, Some(Special(")")), Some('(')) => (ExpressionRecognized, Seq.empty) :: Nil
      case (ExpressionRecognized, Some(Special("}")), Some('{')) => (ExpressionRecognized, Seq.empty) :: Nil
      case (ExpressionRecognized, Some(Special("]")), Some('[')) => (ExpressionRecognized, Seq.empty) :: Nil
      case (ExpressionRecognized, Some(Special("(")), Some(stackSymbol)) => (ExpressionInitial, Seq('(', stackSymbol)) :: Nil
      case (ExpressionRecognized, Some(Special("{")), Some(stackSymbol)) => (ExpressionInitial, Seq('{', stackSymbol)) :: Nil
      case (ExpressionRecognized, Some(Special("[")), Some(stackSymbol)) => (ExpressionInitial, Seq('[', stackSymbol)) :: Nil

      case _ => Seq.empty
    }
  }
}

case class WirthGrammar() extends NonDeterministicPushdownAutomata[WirthLexicalToken, Char, WirthState] {
  override val inputAlphabet: Seq[WirthLexicalToken] = Seq.empty
  override val stackAlphabet: Seq[Char] = CharAlphabets.Alphanumeric.toSeq
  override val initialStackSymbol: Char = '$'
  override val initialState: WirthState = GrammarInit
  override val states: Seq[WirthState] = Seq(GrammarInit, ExpectingEquals, ExpressionInitial, ExpressionRecognized, ExpressionError)
  override val acceptStates: Seq[WirthState] = Seq(RuleRecognized)
  override val trapState: WirthState = ExpressionError

  override def transition[S >: WirthState](state: S, inputSymbolOpt: Option[WirthLexicalToken], stackSymbolOpt: Option[Char]): Seq[(S, Seq[Char])] = {
    val notChanged = stackSymbolOpt.map(s => Seq(s)).getOrElse(Seq.empty)
    (state, inputSymbolOpt, stackSymbolOpt) match {
      case (GrammarInit, Some(NonTerminal(nt)), _) => (ExpectingEquals, notChanged) :: Nil
      case (ExpectingEquals, Some(Special("=")), _) => (ExpressionInitial, notChanged) :: Nil
      case (ExpressionInitial, _, _) => WirthExpression().transition(state, inputSymbolOpt, stackSymbolOpt)
      case (ExpressionRecognized, Some(Special(".")), _) => (RuleRecognized, notChanged) :: Nil
      case (ExpressionRecognized, _, _) => WirthExpression().transition(state, inputSymbolOpt, stackSymbolOpt)
      case (RuleRecognized, None, None) => (GrammarInit, notChanged) :: Nil
      case _ => Seq.empty
    }
  }
}


trait WirthAST
trait Expression extends WirthAST
case class ExpressionParentesis(exp: Expression) extends Expression
case class ExpressionBrackets(exp: Expression) extends Expression
case class ExpressionKleene(exp: Expression) extends Expression
trait Token extends WirthAST
case class NonTerminalToken(nonTerminal: NonTerminal) extends Expression
case class TerminalToken(terminal: Terminal) extends Expression
case class Sequence(expressions: Expression*) extends Expression
case class Or(expressions: Expression*) extends Expression
case class Rule(nonTerminal: NonTerminal, expression: Expression) extends WirthAST
case class Grammar(rules: Rule*) extends WirthAST

//helpers
object NonTerminalToken {
  def apply(str: String): NonTerminalToken = new NonTerminalToken(NonTerminal(str))
}
object TerminalToken {
  def apply(str: String): TerminalToken = new TerminalToken(Terminal(str))
}

object WirthExperimentation {
  import common.automata.SeqStack._
  val openTokens = Seq("(", "[", "{")
  val closeTokens = Seq(")", "]", "}")
  case class NextExpression(next: Seq[WirthLexicalToken], rest: Seq[WirthLexicalToken])
  def getNextExpression(tokenStream: Seq[WirthLexicalToken]): NextExpression = {
    tokenStream.headOption match {
      case Some(t) if t.isInstanceOf[Terminal] || t.isInstanceOf[NonTerminal] =>
        NextExpression(Seq(t), tokenStream.drop(1))
      case Some(Special("|")) =>
        NextExpression(Seq(Special("|")), tokenStream.drop(1))
      case Some(Special(open)) if openTokens.contains(open) =>
//        println("TOKEN: " + open)
        val (_, next) = tokenStream.drop(1).foldLeft((Seq(open), Seq[WirthLexicalToken](Special(open)))) {case ((stack, current), newToken) =>
//          println(stack, current, newToken)
          stack.headOption match {
            case Some(_) =>
              newToken match {
                case Special(o) if openTokens.contains(o) => (stack.push(o), current :+ newToken)
                case Special(c) if closeTokens.contains(c) => (stack.pop(), current :+ newToken)
                case _ => (stack, current :+ newToken)
              }
            case None => (stack, current)
          }
        }
        NextExpression(next, tokenStream.drop(next.size))
      case None => NextExpression(Seq.empty, Seq.empty)
    }
  }
  type TokenStream = Seq[WirthLexicalToken]
  type ExpressionTokenStream = Seq[TokenStream]
  def getExpressions(tokenStream: TokenStream): ExpressionTokenStream = {
    val NextExpression(next, rest) = getNextExpression(tokenStream)
    if (rest.nonEmpty)
      next +: getExpressions(rest)
    else Seq(next)
  }

  def splitByOr(expressionTokenStream: ExpressionTokenStream, existing: Seq[ExpressionTokenStream] = Seq.empty): Seq[ExpressionTokenStream] = {
    expressionTokenStream.indexOf(Seq(Special("|"))) match {
      case -1 => existing :+ expressionTokenStream
      case index =>
        val (right, left) = expressionTokenStream.splitAt(index)
        splitByOr(left.drop(1), existing :+ right)
    }
  }


  private def resolveSeq(seq: Seq[Expression]): Expression = seq match {
    case sequence if sequence.size > 1 => Sequence(sequence: _*)
    case notSequence => notSequence.head
  }

  def parseGrammar(tokenStream: TokenStream): Map[NonTerminalToken, Expression] = {
    tokenStream.indexOf(Special(".")) match {
      case -1 => Map.empty
      case i =>
        val (rule, restGrammar) = tokenStream.splitAt(i)
        parseGrammar(restGrammar.drop(1)) + parseRule(rule)
    }
  }

  def parseRule(tokenStream: TokenStream): (NonTerminalToken, Expression) = {
    tokenStream match {
      case Seq(NonTerminal(rawNT), Arrow, handle @ _*) =>
        NonTerminalToken(rawNT) -> parseHandle(handle)
    }
  }

  def parseHandle(tokenStream: TokenStream): Expression = {
    val expressions: ExpressionTokenStream = getExpressions(tokenStream)
    splitByOr(expressions) match {
      case or if or.size > 1 =>
        val orExpressions = or.map { orStatement =>
          resolveSeq(orStatement.map(parseHandle))
        }
        Or(orExpressions: _*)
      case noOr =>
        val expressions: ExpressionTokenStream = noOr.head
        resolveSeq {
          expressions.map {
            case Seq(nt: NonTerminal) => NonTerminalToken(nt)
            case Seq(t: Terminal) => TerminalToken(t)
            case parentesis: Seq[WirthLexicalToken] if parentesis.headOption.contains(Special("(")) && parentesis.lastOption.contains(Special(")")) => ExpressionParentesis(parseHandle(parentesis.drop(1).dropRight(1)))
            case brackets: Seq[WirthLexicalToken] if brackets.headOption.contains(Special("[")) && brackets.lastOption.contains(Special("]")) => ExpressionBrackets(parseHandle(brackets.drop(1).dropRight(1)))
            case kleenes: Seq[WirthLexicalToken] if kleenes.headOption.contains(Special("{")) && kleenes.lastOption.contains(Special("}")) => ExpressionKleene(parseHandle(kleenes.drop(1).dropRight(1)))
          }
        }
    }
  }

  def nonTerminalsOfExpression(e: Expression): Seq[NonTerminalToken] = {
//    println(e)
    e match {
      case or: Or => or.expressions.flatMap(nonTerminalsOfExpression)
      case seq: Sequence => seq.expressions.flatMap(nonTerminalsOfExpression)
      case parentesis: ExpressionParentesis => nonTerminalsOfExpression(parentesis.exp)
      case brackets: ExpressionBrackets => nonTerminalsOfExpression(brackets.exp)
      case kleene: ExpressionKleene => nonTerminalsOfExpression(kleene.exp)
      case nt: NonTerminalToken => Seq(nt)
      case _: TerminalToken => Seq.empty
    }
  }

  def replace(e: Expression, nt: NonTerminalToken, replaceExpression: Expression): Expression = {
//    println(s"exp: $e")
    e match {
      case or: Or => Or(or.expressions.map(orExp => replace(orExp, nt, replaceExpression)): _*)
      case seq: Sequence => Sequence(seq.expressions.map(seqExp => replace(seqExp, nt, replaceExpression)): _*)
      case parentesis: ExpressionParentesis => ExpressionParentesis(replace(parentesis.exp, nt, replaceExpression))
      case brackets: ExpressionBrackets => ExpressionBrackets(replace(brackets.exp, nt, replaceExpression))
      case kleene: ExpressionKleene => ExpressionKleene(replace(kleene.exp, nt, replaceExpression))
      case replaced: NonTerminalToken if replaced == nt => replaceExpression
      case nt: NonTerminalToken => nt
      case t: TerminalToken => t
    }
  }

  def reduceRules(rules: Map[NonTerminalToken, Expression]): Map[NonTerminalToken, Expression] = {
    rules.toSeq.foldLeft(Map.empty[NonTerminalToken, Expression]) { case (newRules, (ntt, expression)) =>
        newRules + (ntt -> nonTerminalsOfExpression(expression).filterNot(_ == ntt).foldLeft(expression) {case (exp, nttd) =>
//          println {
//            s"""Replacing expression
//              |${exp}
//              |Looking for $nttd
//              |to ${rules.getOrElse(nttd, Or())}
//              |and we got ${replace(exp, nttd, rules.getOrElse(nttd, Or()))}
//            """.stripMargin
//          }
            replace(exp, nttd, rules.getOrElse(nttd, Or()))
          })
    }
  }

  def expToString(exp: Expression): String = exp match {
    case or: Or => s"( ${or.expressions.map(expToString).mkString(" | ")} )"
    case seq: Sequence => seq.expressions.map(expToString).mkString(" ")
    case parentesis: ExpressionParentesis => s"( ${expToString(parentesis.exp)} )"
    case brackets: ExpressionBrackets => s"[ ${expToString(brackets.exp)} ]"
    case kleene: ExpressionKleene => s"{ ${expToString(kleene.exp)} }"
    case nt: NonTerminalToken => nt.nonTerminal.str
    case t: TerminalToken => t.terminal.str
  }

  def printRules(rules: Map[NonTerminalToken, Expression]): Unit = {
    rules.foreach { case (ntt, exp) =>
        println(s"${ntt.nonTerminal.str} -> ${expToString(exp)}")
    }
  }
}

object WirthToNDPA {

  sealed trait StackAlphabet
  case object InitialStackSymbol extends StackAlphabet
  case class CentralAutoRecursion(id: String) extends StackAlphabet

  sealed trait WirthGeneratedState
  case class TrapState(id: String, exp: Expression) extends WirthGeneratedState
  case class RuleInitialState(nonTerminalToken: NonTerminalToken) extends WirthGeneratedState
  case class RuleAcceptState(nonTerminalToken: NonTerminalToken) extends WirthGeneratedState
  case class PartialSequence(id: String, seq: Sequence, done: Int) extends WirthGeneratedState
  case class AcceptSequence(id: String, seq: Sequence) extends WirthGeneratedState
  case class WaitingTerminal(id: String, terminalToken: TerminalToken) extends WirthGeneratedState
  case class AcceptTerminal(id: String, terminalToken: TerminalToken) extends WirthGeneratedState
  def fromTerminal(tt: TerminalToken): NonDeterministicPushdownAutomata[WirthLexicalToken, StackAlphabet, WirthGeneratedState] = {
    fromTerminal(tt, randomUUID().toString)
  }

  def fromTerminal(tt: TerminalToken, uuid: String): NonDeterministicPushdownAutomata[WirthLexicalToken, StackAlphabet, WirthGeneratedState] = {
    new NonDeterministicPushdownAutomata[WirthLexicalToken, StackAlphabet, WirthGeneratedState] {
      override val id: String = uuid
      override val inputAlphabet: Seq[WirthLexicalToken] = Seq(tt.terminal)
      override val stackAlphabet: Seq[StackAlphabet] = Seq(InitialStackSymbol)
      override val initialStackSymbol: StackAlphabet = InitialStackSymbol
      override val initialState: WirthGeneratedState = WaitingTerminal(id, tt)
      override val states: Seq[WirthGeneratedState] = Seq(WaitingTerminal(id, tt), AcceptTerminal(id, tt), TrapState(id, tt))
      override val acceptStates: Seq[WirthGeneratedState] = Seq(AcceptTerminal(id, tt))
      override val trapState: WirthGeneratedState = TrapState(id, tt)

      override def transition[S >: WirthGeneratedState](state: S, inputSymbolOpt: Option[WirthLexicalToken], stackSymbolOpt: Option[StackAlphabet]): Seq[(S, Seq[StackAlphabet])] = {
        val notChanged = stackSymbolOpt.map(s => Seq(s)).getOrElse(Seq.empty)
        val x = (state, inputSymbolOpt, stackSymbolOpt) match {
          case (WaitingTerminal(i, _), Some(tt.terminal), Some(InitialStackSymbol)) if i == id => (AcceptTerminal(id, tt), Seq.empty) :: Nil
          case _ => Seq.empty
        }
        x
      }
    }
  }

  def fromSequenceOfTerminals(seq: Sequence): NonDeterministicPushdownAutomata[WirthLexicalToken, StackAlphabet, WirthGeneratedState] = {
    fromSequenceOfTerminals(seq, randomUUID().toString)
  }

  def fromSequenceOfTerminals(seq: Sequence, uuid: String): NonDeterministicPushdownAutomata[WirthLexicalToken, StackAlphabet, WirthGeneratedState] = {
    require(seq.expressions.forall(e => e.isInstanceOf[TerminalToken]), "SequenceOfTerminals was call and not every expression is a TerminalToken")
    val terminals = seq.expressions.map {case t: TerminalToken => t.terminal}
    new NonDeterministicPushdownAutomata[WirthLexicalToken, StackAlphabet, WirthGeneratedState] {
      override val id: String = uuid
      override val inputAlphabet: Seq[WirthLexicalToken] = Seq.empty
      override val stackAlphabet: Seq[StackAlphabet] = Seq.empty
      override val initialStackSymbol: StackAlphabet = InitialStackSymbol
      override val initialState: WirthGeneratedState = PartialSequence(uuid, seq, 0)
      override val states: Seq[WirthGeneratedState] = (0 to seq.expressions.size).map(i => PartialSequence(uuid, seq, i)) ++ Seq(AcceptSequence(uuid, seq), TrapState(uuid, seq))
      override val acceptStates: Seq[WirthGeneratedState] = Seq(AcceptSequence(uuid, seq))
      override val trapState: WirthGeneratedState = TrapState(uuid, seq)

      override def transition[S >: WirthGeneratedState](state: S, inputSymbolOpt: Option[WirthLexicalToken], stackSymbolOpt: Option[StackAlphabet]): Seq[(S, Seq[StackAlphabet])] = {
        val notChanged = stackSymbolOpt.map(s => Seq(s)).getOrElse(Seq.empty)
        val x = (state, inputSymbolOpt, stackSymbolOpt) match {
          case (PartialSequence(_, sequence, i), Some(input), _) if sequence == seq && terminals.lift(i).contains(input) && terminals.size == i+1 => (AcceptSequence(uuid, seq), Seq.empty) :: Nil
          case (PartialSequence(_, sequence, i), Some(input), _) if sequence == seq && terminals.lift(i).contains(input) => (PartialSequence(uuid, seq, i+1) ,notChanged) :: Nil
//          case (AcceptSequence(sequence), None, Some(InitialStackSymbol)) if sequence == seq => (AcceptSequence(seq), Seq.empty) :: Nil
          case _ => Seq.empty
        }
        x
      }
    }
  }

  case class ExpressionContext(callStack: Seq[NonTerminalToken], rules: Map[NonTerminalToken, Expression]) {
    import common.automata.SeqStack._
    val dependencyGraph: Map[NonTerminalToken, Seq[NonTerminalToken]] = rules.map {case (ntt, exp) => ntt -> WirthExperimentation.nonTerminalsOfExpression(exp)}
    def getRule(exp: Expression): Option[NonTerminalToken] = rules.map(_.swap).get(exp)
    def isRecursive(ntt: NonTerminalToken): Boolean = callStack.contains(ntt)
    def isRecursiveWithoutContext(ntt: NonTerminalToken, searchRecursive: Seq[NonTerminalToken] = Seq.empty): Boolean = {
      val nextSearch = (searchRecursive :+ ntt).flatMap(dependencyGraph).distinct
      (searchRecursive :+ ntt).exists(s => dependencyGraph(s).contains(ntt)) || (if(nextSearch.toSet.subsetOf(searchRecursive.toSet) && searchRecursive.toSet.subsetOf(nextSearch.toSet)) false else isRecursiveWithoutContext(ntt, nextSearch))
    }
    def call(nonTerminalToken: NonTerminalToken): ExpressionContext = copy(callStack.push(nonTerminalToken))
  }


  def isRecursionSafe(exp: Expression, context: ExpressionContext): Boolean = {
    val nttIsRecSafe : PartialFunction[Expression, Boolean] = {
      case ntt: NonTerminalToken => isRecursionSafe(ntt, context.call(ntt))
      case e => isRecursionSafe(e, context)
    }
    val x = exp match {
      case ntt: NonTerminalToken => !context.isRecursiveWithoutContext(ntt)
      case _: TerminalToken => true
      case seq: Sequence => seq.expressions.forall(nttIsRecSafe)
      case or: Or => or.expressions.forall(nttIsRecSafe)
      case expressionKleene: ExpressionKleene => isRecursionSafe(expressionKleene.exp, context)
      case expressionBrackets: ExpressionBrackets => isRecursionSafe(expressionBrackets.exp, context)
      case expressionParentesis: ExpressionParentesis => isRecursionSafe(expressionParentesis.exp, context)
    }
    x
  }

  def fromNonRecursiveExpression(exp: Expression, context: ExpressionContext): NonDeterministicPushdownAutomata[WirthLexicalToken, StackAlphabet, WirthGeneratedState] = {
    exp match {
      case ntt: NonTerminalToken => fromNonRecursiveExpression(context.rules(ntt), context.call(ntt))
      case tt: TerminalToken => fromTerminal(tt)
      case seq: Sequence if seq.expressions.forall(e =>  e.isInstanceOf[TerminalToken]) => fromSequenceOfTerminals(seq)
      case seq: Sequence =>
        seq.expressions.map(exp => fromNonRecursiveExpression(exp, context)).reduceLeft(NonDeterministicPushdownAutomata.concat)
      case or: Or =>
        or.expressions.map(exp => fromNonRecursiveExpression(exp, context)).reduceLeft(NonDeterministicPushdownAutomata.or)
      case expressionKleene: ExpressionKleene =>
        NonDeterministicPushdownAutomata.kleene(fromNonRecursiveExpression(expressionKleene.exp, context))
      case expressionBrackets: ExpressionBrackets =>
        val ndpa = fromNonRecursiveExpression(expressionBrackets.exp, context)
        ndpa.addTransition(ndpa.initialState, None, None, Seq((ndpa.acceptStates.head, Seq.empty)))
      case expressionParentesis: ExpressionParentesis => ???
        fromNonRecursiveExpression(expressionParentesis.exp, context)
    }
  }

  def fromExpressionWithoutRecursion(exp: Expression, context: ExpressionContext): NonDeterministicPushdownAutomata[WirthLexicalToken, StackAlphabet, WirthGeneratedState] = {
    println("fromExpressionWithoutRecursion")
    val x = exp match {
      case seq: Sequence if seq.expressions.forall(e =>  e.isInstanceOf[TerminalToken]) =>
        fromSequenceOfTerminals(seq)
      case seq: Sequence if seq.expressions.forall(e => isRecursionSafe(e, context)) =>
        fromNonRecursiveExpression(seq, context)
      case terminalToken: TerminalToken =>
        fromTerminal(terminalToken)
      case or: Or =>
        val ndpas: Seq[NonDeterministicPushdownAutomata[WirthLexicalToken, StackAlphabet, WirthGeneratedState]] = or.expressions.filterNot(e => WirthExperimentation.nonTerminalsOfExpression(e).exists(exp => context.isRecursive(exp))).map(o => fromExpression(o, context))
        ndpas.drop(1).foldLeft(ndpas.head) {case (ndpa1, ndpa2) => NonDeterministicPushdownAutomata.or(ndpa1, ndpa2)}
    }
    x
  }

  case class CentralRecursion[StackSymbol](leftMap: Map[StackSymbol,Expression],
                                           rightMap: Map[StackSymbol, Expression]) {
    def ++(centralRecursion: CentralRecursion[StackSymbol]): CentralRecursion[StackSymbol] = {
      CentralRecursion(
        leftMap ++ centralRecursion.leftMap,
        rightMap ++ centralRecursion.rightMap)
    }
  }
  def splitRecursion[StackSymbol](exp: Expression, exp2stackSymbol: Expression => StackSymbol): CentralRecursion[StackSymbol] = {
    exp match {
      case seq: Sequence if seq.expressions.exists(e => e.isInstanceOf[NonTerminalToken]) =>
        val left = seq.expressions.takeWhile(e => e.isInstanceOf[TerminalToken])
        val (Seq(ntt), right) = seq.expressions.drop(left.size).splitAt(1)
        CentralRecursion(
          Map(exp2stackSymbol(ntt) -> Sequence(left: _*)),
          Map(exp2stackSymbol(ntt) -> Sequence(right: _*)))
      case or: Or =>
        or.expressions.map(exp => splitRecursion(exp, exp2stackSymbol)).reduce(_ ++ _)
    }
  }
  def applyCentralRecursion(ndpa: NonDeterministicPushdownAutomata[WirthLexicalToken, StackAlphabet, WirthGeneratedState],
                            centralRecursion: CentralRecursion[StackAlphabet],
                            context: ExpressionContext): NonDeterministicPushdownAutomata[WirthLexicalToken, StackAlphabet, WirthGeneratedState] = {

    val leftNdpas = centralRecursion.leftMap.map {case (ntt, exp) => ntt -> fromExpressionWithoutRecursion(exp, context)}
    val rightNdpas = centralRecursion.rightMap.map {case (ntt, exp) => ntt -> fromExpressionWithoutRecursion(exp, context)}

    ndpa.copy(
      newStates = ndpa.states ++ leftNdpas.values.flatMap(_.states) ++ rightNdpas.values.flatMap(_.states),
      newTransition = { (state: WirthGeneratedState, optInput, optStack) =>
      val oldTransitions = ndpa.transition(state, optInput, optStack)
      (state, optInput, optStack) match {
        case (s, None, Some(stack)) if (stack == ndpa.initialStackSymbol || leftNdpas.keySet.contains(stack)) && s == ndpa.initialState => // entering left expression
          println(s"entering left expression")
          oldTransitions ++ leftNdpas.values.map(legtNdpa => (legtNdpa.initialState, Seq(legtNdpa.initialStackSymbol, stack)))
        case (s, None, None) if ndpa.acceptStates.contains(s) => // entering right expression
          println(s"entering right expression $optInput, $optStack")
          oldTransitions ++ rightNdpas.values.map(rightNdpa => (rightNdpa.initialState, Seq()))
        case (s, _, _) if leftNdpas.values.flatMap(_.acceptStates).toSeq.contains(s) => // leaving left expression
          println(s"leaving left expression")
          oldTransitions ++ (
            for {
              (stackSymbol, leftNdpa) <- leftNdpas.toSeq if leftNdpa.acceptStates.contains(s)
            } yield (ndpa.initialState, Seq(stackSymbol)))
        case (s, _, Some(stackSymbol)) if rightNdpas.values.flatMap(_.acceptStates).toSeq.contains(s) && rightNdpas.keys.toSeq.contains(stackSymbol) => //leaving right expression
          println(s"leaving right expression")
          oldTransitions ++ ndpa.acceptStates.map{ acceptState => (acceptState, Seq.empty)}
        case _ =>
          println(s"running for input $optInput, $optStack")
          oldTransitions ++
            leftNdpas.values.flatMap(l => l.transition(state, optInput, optStack)) ++
            rightNdpas.values.flatMap(r => r.transition(state, optInput, optStack))
      }
    })
  }
  def fromExpression(exp: Expression, context: ExpressionContext): NonDeterministicPushdownAutomata[WirthLexicalToken, StackAlphabet, WirthGeneratedState] = {
    println(s"fromExpression: ${WirthExperimentation.expToString(exp)}")
    val ndpa = exp match {
      case seq: Sequence if seq.expressions.forall(e =>  e.isInstanceOf[TerminalToken]) =>
        fromSequenceOfTerminals(seq)
      case seq: Sequence if  seq.expressions.forall(e => isRecursionSafe(e, context)) => fromNonRecursiveExpression(seq, context)
      case terminalToken: TerminalToken =>
        fromTerminal(terminalToken)
      case seq: Sequence if seq.expressions.exists(e => e.isInstanceOf[NonTerminalToken]) =>
        val left = seq.expressions.takeWhile(e => e.isInstanceOf[TerminalToken])
        val (Seq(ntt: NonTerminalToken), right) = seq.expressions.drop(left.size).splitAt(1)
//        val ndpaFromNTT = fromExpression(context.rules(ntt), context.call(ntt))
        val ruleInitialState = RuleInitialState(ntt)
        (left, right) match {
          case (l, r) if l.isEmpty && r.isEmpty => throw new Exception("empty on both sides")
          case (l, r) if l.isEmpty => //left recursion
            val prefix = fromExpressionWithoutRecursion(context.rules(ntt), context)
            val repeat = NonDeterministicPushdownAutomata.kleene(fromExpression(Sequence(r: _*), context))
            NonDeterministicPushdownAutomata.concat(
              prefix,
              repeat
            ).addTransition(prefix.acceptStates, None, Some(InitialStackSymbol), Seq((repeat.acceptStates.head, Seq.empty)))
          case (l, r) if r.isEmpty => // right recursion
            val repeat = NonDeterministicPushdownAutomata.kleene(fromExpression(Sequence(l: _*), context))
            val suffix = fromExpressionWithoutRecursion(context.rules(ntt), context)
            NonDeterministicPushdownAutomata.concat(
              repeat,
              suffix
            )//.addTransition(suffix.acceptStates, None, Some(InitialStackSymbol), Seq((suffix.acceptStates.head, Seq.empty)))
          case (l, r) if context.isRecursive(ntt) =>
            println("Auto recursion")
            println(ntt, l, r)
            val leftExp = Sequence(l: _*)
            val rightExp = Sequence(r: _*)
            val centerAutomata = fromExpressionWithoutRecursion(context.rules(ntt), context)
            val leftAutomata = fromExpression(leftExp, context)
            val rightAutomata = fromExpression(rightExp, context)
            val recursion = CentralAutoRecursion(WirthExperimentation.expToString(leftExp))
            println("parts done")
            NonDeterministicPushdownAutomata.concat(
              NonDeterministicPushdownAutomata.concat(
                leftAutomata
                  .addState(ruleInitialState)
                  .addTransition(leftAutomata.acceptStates, None, None, Seq((ruleInitialState, Seq(recursion)))),
                centerAutomata
              ),
              rightAutomata
                .addTransition(rightAutomata.acceptStates, None, Some(recursion), Seq((rightAutomata.acceptStates.head, Seq.empty)))
            )
        }
      case or: Or =>
        val ndpas: Seq[NonDeterministicPushdownAutomata[WirthLexicalToken, StackAlphabet, WirthGeneratedState]] = or.expressions.map(o => fromExpression(o, context))
        ndpas.reduceLeft[NonDeterministicPushdownAutomata[WirthLexicalToken, StackAlphabet, WirthGeneratedState]]
          {case (ndpa1, ndpa2) =>
            NonDeterministicPushdownAutomata.or(ndpa1, ndpa2)}
      case a =>
        println("PANIC")
        println(a)
        ???
    }
    println("wat")
    context.getRule(exp) match {
      case Some(ntt) =>
        val ruleInitialState = RuleInitialState(ntt)
        val ruleAcceptState = RuleAcceptState(ntt)
        ndpa
          .addState(ruleInitialState)
          .addState(ruleAcceptState)
          .replaceInitialState(ruleInitialState)
          .replaceAcceptStates(Seq(ruleAcceptState))
          .addTransition(ruleInitialState, None, None, Seq((ndpa.initialState, Seq(InitialStackSymbol))))
          .addTransition(ndpa.acceptStates, None, Some(InitialStackSymbol), Seq((ruleAcceptState, Seq.empty)))
      case None =>
        ndpa
    }
  }

  def fromRules(rules: Map[NonTerminalToken, Expression], nonTerminalInitial: NonTerminalToken): NonDeterministicPushdownAutomata[WirthLexicalToken, StackAlphabet, WirthGeneratedState] = {
    fromExpression(rules(nonTerminalInitial), ExpressionContext(Seq(nonTerminalInitial), rules))
  }
}
