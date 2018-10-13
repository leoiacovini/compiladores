package wirth

import automata.CharAlphabets
import automata.ndpa.NonDeterministicPushdownAutomata
import wirth.WirthNotation._

trait WirthStuff
case class Terminal(str: String) extends WirthStuff
case class NonTerminal(str: String) extends WirthStuff
case class Special(str: String) extends WirthStuff
case object EmptySequence extends WirthStuff

object WirthNotation {
  sealed trait WirthState
  case object GrammarInit extends WirthState
  case object ExpectingEquals extends WirthState
  case object RuleRecognized extends WirthState
  case object ExpressionInitial extends WirthState
  case object ExpressionRecognized extends WirthState
  case object ExpressionError extends WirthState

}

case class WirthExpression() extends NonDeterministicPushdownAutomata[WirthStuff, Char, WirthState] {
  override val inputAlphabet: Seq[WirthStuff] = Seq.empty
  override val stackAlphabet: Seq[Char] = CharAlphabets.Alphanumeric.toSeq
  override val initialStackSymbol: Char = '$'
  override val initialState: WirthState = ExpressionInitial
  override val states: Seq[WirthState] = Seq(ExpressionInitial, ExpressionRecognized, ExpressionError)
  override val acceptStates: Seq[WirthState] = Seq(ExpressionRecognized)
  override val trapState: WirthState = ExpressionError

  override def transition[S >: WirthState](state: S, inputSymbolOpt: Option[WirthStuff], stackSymbolOpt: Option[Char]): Seq[(S, Seq[Char])] = {
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

case class WirthGrammar() extends NonDeterministicPushdownAutomata[WirthStuff, Char, WirthState] {
  override val inputAlphabet: Seq[WirthStuff] = Seq.empty
  override val stackAlphabet: Seq[Char] = CharAlphabets.Alphanumeric.toSeq
  override val initialStackSymbol: Char = '$'
  override val initialState: WirthState = GrammarInit
  override val states: Seq[WirthState] = Seq(GrammarInit, ExpectingEquals, ExpressionInitial, ExpressionRecognized, ExpressionError)
  override val acceptStates: Seq[WirthState] = Seq(RuleRecognized)
  override val trapState: WirthState = ExpressionError

  override def transition[S >: WirthState](state: S, inputSymbolOpt: Option[WirthStuff], stackSymbolOpt: Option[Char]): Seq[(S, Seq[Char])] = {
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
case class NonTerminalToken(nonTerminal: NonTerminal)
case class TerminalToken(terminal: Terminal)
case class Sequence(tokens: Token*)
case class Or(expressions: Expression*) extends Expression
case class Rule(nonTerminal: NonTerminal, expression: Expression) extends WirthAST
case class Grammar(rules: Rule*) extends WirthAST
