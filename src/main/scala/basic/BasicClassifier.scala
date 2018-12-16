package basic

import common.automata.LexicalToken

object BasicClassifier {

  def classifyToken(lexicalToken: LexicalToken): BasicToken = {
    lexicalToken.rawValue.toUpperCase() match {
      case "LET" => BasicToken.Let()
      case "RETURN" => BasicToken.Return()
      case "GOSUB" => BasicToken.GoSub()
      case "READ" => BasicToken.Read()
      case "DATA" => BasicToken.Data()
      case "PRINT" => BasicToken.Print()
      case "FOR" => BasicToken.For()
      case "NEXT" => BasicToken.Next()
      case "REM" => BasicToken.Rem()
      case "THEN" => BasicToken.Then()
      case "FN" => BasicToken.Fn()
      case "STEP" => BasicToken.Step()
      case "TO" => BasicToken.To()
      case "DIM" => BasicToken.Dim()
      case "END" => BasicToken.End()
      case "DEF" => BasicToken.Def()
    }
  }

  def classifyLine(lexicalSeq: Seq[LexicalToken]): Seq[BasicToken] = {
    val lineNumber = lexicalSeq.head
    val commandLine = lexicalSeq.tail
    commandLine.map(classifyToken) :+ BasicToken.LineNumber(lineNumber.rawValue)
  }

  def createStatement(lexicalTokens: Seq[LexicalToken]): BasicStatement = {
    BasicStatement.fromTokensLine(classifyLine(lexicalTokens))
  }

}
