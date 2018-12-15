package basic

trait BasicCommand
object BasicCommand {

  case class Assign(varName: BasicToken.Identifier, exp: Seq[BasicToken])
  case class Print(items: Seq[BasicToken])
  case class Goto(lineNumber: BasicToken.Number)
  case class If(exp1: Seq[BasicToken], exp2: Seq[BasicToken], compareOperator: BasicToken.Operator, thenInt: BasicToken.Number)
  case class Read(vars: Seq[BasicToken.Identifier])
  case class Data(values: Seq[BasicToken])
  case class For(varName: BasicToken.Identifier, to: BasicToken, step: BasicToken)
  case class Next(varName: BasicToken.Identifier)
  case class GoSub(number: BasicToken.Number)
  case class Return()
  case class Remark(comment: BasicToken.Text)


  def tokensLineToAssign(commandLine: Seq[BasicToken]): BasicCommand.Assign = commandLine match {
    case Seq(_let, varName: BasicToken.Identifier, _equal, tail @ _*) => Assign(varName, tail)
  }

  def tokensLineToRead(commandLine: Seq[BasicToken]): BasicCommand.Read = commandLine match {
    case Seq(_read, tail @ _*) => Read(tail.collect { case id: BasicToken.Identifier => id })
  }

  def tokensLineToPrint(commandLine: Seq[BasicToken]): BasicCommand.Print = commandLine match {
    case Seq(_print, tail @ _*) => Print(tail)
  }

  def tokensLineToData(commandLine: Seq[BasicToken]): BasicCommand.Data = commandLine match {
    case Seq(_data, tail @ _*) => Data(tail)
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

  def tokensLineToIf(commandLine: Seq[BasicToken]): BasicCommand.If = {
    val withoutIf = commandLine.tail
    val operatorIndex = withoutIf.indexOf(BasicToken.Operator(_))
    val operatorSplit = withoutIf.splitAt(operatorIndex)
    val exp1 = operatorSplit._1
    val restAfterOperator = operatorSplit._2.tail
    val thenIndex = restAfterOperator.indexOf(BasicToken.Then(_))
    val thenSplit = restAfterOperator.splitAt(thenIndex)
    val exp2 = thenSplit._1
    val thenInt = thenSplit._2(1)
    BasicCommand.If(exp1, exp2, withoutIf(operatorIndex).asInstanceOf[BasicToken.Operator], thenInt.asInstanceOf[BasicToken.Number])
  }

  def fromTokensLine(commandLine: Seq[BasicToken]): BasicCommand = {
    val command = commandLine.head match {
      case BasicToken.Let(_) => tokensLineToAssign(commandLine)
      case BasicToken.Read(_) => tokensLineToRead(commandLine)
      case BasicToken.Print(_) => tokensLineToPrint(commandLine)
      case BasicToken.Data(_) => tokensLineToData(commandLine)
      case BasicToken.Goto(_) => tokensLineToGoto(commandLine)
      case BasicToken.Return(_) => tokensLineToReturn(commandLine)
      case BasicToken.GoSub(_) => tokensLineToGosub(commandLine)
      case BasicToken.Next(_) => tokensLineToNext(commandLine)
      case BasicToken.If(_) => tokensLineToIf(commandLine)
    }
    command.asInstanceOf[BasicCommand]
  }
}
