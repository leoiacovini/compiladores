package basic

case class BasicStatement(lineNumber: Int, command: BasicCommand)

object BasicStatement {

  def fromTokensLine(tokensLine: Seq[BasicToken]): BasicStatement = {
    BasicStatement(
      tokensLine.head.literal.toInt,
      BasicCommand.fromTokensLine(tokensLine.tail)
    )
  }

}