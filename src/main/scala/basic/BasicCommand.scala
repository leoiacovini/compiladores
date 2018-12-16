package basic

import scala.collection.mutable.ListBuffer

trait BasicCommand
object BasicCommand {

  case class Assign(varName: BasicToken.Identifier, exp: Expression) extends BasicCommand
  case class Print(items: Seq[Expression]) extends BasicCommand
  case class Goto(lineNumber: BasicToken.Number) extends BasicCommand
  case class If(exp1: Expression, exp2: Expression, comparator: BasicToken.Comparator, thenInt: BasicToken.Number) extends BasicCommand
  case class Read(vars: Seq[BasicToken.Identifier]) extends BasicCommand
  case class Data(values: Seq[BasicToken]) extends BasicCommand
  case class For(varName: BasicToken.Identifier, initialExp: Expression, toExp: Expression, step: Option[Expression] = None) extends BasicCommand
  case class Next(varName: BasicToken.Identifier) extends BasicCommand
  case class GoSub(number: BasicToken.Number) extends BasicCommand
  case class Return() extends BasicCommand
  case class Remark(comment: BasicToken.Text) extends BasicCommand

  def splitBySeparator[T]( l: Seq[T], sep: T ): Seq[Seq[T]] = {
    val b = ListBuffer(ListBuffer[T]())
    l foreach { e =>
      if ( e == sep ) {
        if  ( b.last.nonEmpty ) b += ListBuffer[T]()
      }
      else b.last += e
    }
    b.map(_.toSeq)
  }

  def tokensLineToAssign(commandLine: Seq[BasicToken]): BasicCommand.Assign = commandLine match {
    case Seq(_let, varName: BasicToken.Identifier, _equal, tail @ _*) => Assign(varName, Expression(tail:_*))
  }

  def tokensLineToRead(commandLine: Seq[BasicToken]): BasicCommand.Read = commandLine match {
    case Seq(_read, tail @ _*) => Read(tail.collect { case id: BasicToken.Identifier => id })
  }

  def tokensLineToPrint(commandLine: Seq[BasicToken]): BasicCommand.Print = commandLine match {
    case Seq(_print, tail @ _*) =>
      val items = splitBySeparator[BasicToken](tail.toList, BasicToken.Delimiter())
      Print(items.map(i => Expression(i:_*)))
  }

  def tokensLineToData(commandLine: Seq[BasicToken]): BasicCommand.Data = commandLine match {
    case Seq(_data, tail @ _*) =>
      val items = tail.partition(b => b.isInstanceOf[BasicToken.Delimiter])
      Data(items._2)
  }

  def tokensLineToGoto(commandLine: Seq[BasicToken]): BasicCommand.Goto = commandLine match {
    case Seq(_goto, lineNumber: BasicToken.Number) => Goto(lineNumber)
  }

  def tokensLineToReturn(commandLine: Seq[BasicToken]): BasicCommand.Return = commandLine match {
    case Seq(_return) => Return()
  }

  def tokensLineToGosub(commandLine: Seq[BasicToken]): BasicCommand.GoSub = commandLine match {
    case Seq(_gosub, lineNumber: BasicToken.Number) => GoSub(lineNumber)
  }

  def tokensLineToNext(commandLine: Seq[BasicToken]): BasicCommand.Next = commandLine match {
    case Seq(_next, varName: BasicToken.Identifier) => Next(varName)
  }

  def tokensLineToFor(commandLine: Seq[BasicToken]): BasicCommand.For = commandLine match {
    case Seq(_for, varName: BasicToken.Identifier, _equal: BasicToken.Equal, tail @_*) =>
      val toIndex = tail.indexOf(BasicToken.To())
      val (exp1, exp2PlusTo) = tail.splitAt(toIndex)

      val exp2AndStep = exp2PlusTo.tail
      val stepIndex = exp2AndStep.indexOf(BasicToken.Step())
      if (stepIndex == -1) {
        For(varName, Expression(exp1:_*), Expression(exp2PlusTo.tail:_*))
      } else {
        val (exp2, stepExp) = exp2AndStep.splitAt(stepIndex)
        For(varName, Expression(exp1:_*), Expression(exp2:_*), Some(Expression(stepExp.tail:_*)))
      }

  }

  def tokensLineToIf(commandLine: Seq[BasicToken]): BasicCommand.If = {
    val withoutIf = commandLine.tail
    val comparator = withoutIf.collectFirst { case comp: BasicToken.Comparator => comp }.get
    val comparatorIndex = withoutIf.indexOf(comparator)
    val operatorSplit = withoutIf.splitAt(comparatorIndex)
    val exp1 = operatorSplit._1
    val restAfterOperator = operatorSplit._2.tail
    val thenIndex = restAfterOperator.indexOf(BasicToken.Then())
    val thenSplit = restAfterOperator.splitAt(thenIndex)
    val exp2 = thenSplit._1
    val thenInt = thenSplit._2(1)
    BasicCommand.If(Expression(exp1:_*), Expression(exp2:_*), comparator, thenInt.asInstanceOf[BasicToken.Number])
  }

  def fromTokensLine(commandLine: Seq[BasicToken]): BasicCommand = {
    commandLine.head match {
      case BasicToken.Let(_) => tokensLineToAssign(commandLine)
      case BasicToken.Read(_) => tokensLineToRead(commandLine)
      case BasicToken.Print(_) => tokensLineToPrint(commandLine)
      case BasicToken.Data(_) => tokensLineToData(commandLine)
      case BasicToken.Goto(_) => tokensLineToGoto(commandLine)
      case BasicToken.Return(_) => tokensLineToReturn(commandLine)
      case BasicToken.GoSub(_) => tokensLineToGosub(commandLine)
      case BasicToken.Next(_) => tokensLineToNext(commandLine)
      case BasicToken.If(_) => tokensLineToIf(commandLine)
      case BasicToken.For(_) => tokensLineToFor(commandLine)
      case BasicToken.Rem(_) => BasicCommand.Remark(BasicToken.Text(""))
    }
  }
}
