package basic

import common.automata.LexicalToken

object BasicClassifier {

  def classifyToken(lexicalToken: LexicalToken): BasicToken = {

    lexicalToken.typ.toUpperCase match {
      case "NUMBER" => BasicToken.Number(lexicalToken.rawValue)
      case "INTEGER" => BasicToken.Number(lexicalToken.rawValue)
      case "STRING" => BasicToken.Text(lexicalToken.rawValue.drop(1).dropRight(1))
      case "VARIABLENAME" => BasicToken.Identifier(lexicalToken.rawValue)
      case _ => lexicalToken.rawValue.toUpperCase() match {
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
        case "(" => BasicToken.OpenParenthesis()
        case ")" => BasicToken.CloseParenthesis()
        case "+" => BasicToken.Plus()
        case "-" => BasicToken.Minus()
        case "*" => BasicToken.Multiply()
        case "/" => BasicToken.Divide()
        case "=" => BasicToken.Equal()
        case ">" => BasicToken.Greater()
        case "<" => BasicToken.Lesser()
        case "<>" => BasicToken.Different()
        case "," => BasicToken.Delimiter()
      }
    }
  }

  def classifyLine(lexicalSeq: Seq[LexicalToken]): Seq[BasicToken] = {
    val lineNumber = lexicalSeq.head
    val commandLine = lexicalSeq.tail
    BasicToken.LineNumber(lineNumber.rawValue) +: commandLine.map(classifyToken)
  }

  def createStatement(lexicalTokens: Seq[LexicalToken]): BasicStatement = {
    BasicStatement.fromTokensLine(classifyLine(lexicalTokens))
  }

}
