package basic

class Classifier {

  def classifyToken(literal: String): BasicToken = {
    literal.toUpperCase() match {
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
      case _ => BasicToken.Keyword(literal)
    }
  }

}
