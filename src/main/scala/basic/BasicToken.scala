package basic

trait BasicToken {
  val literal: String
}

object BasicToken {

  // Keywords
  abstract class Keyword(literal: String) extends BasicToken

  case class Read(override val literal: String = "READ") extends Keyword(literal)

  case class Data(override val literal: String = "DATA") extends Keyword(literal)

  case class Print(override val literal: String = "PRINT") extends Keyword(literal)

  case class Goto(override val literal: String = "GOTO") extends Keyword(literal)

  case class For(override val literal: String = "FOR") extends Keyword(literal)

  case class Next(override val literal: String = "NEXT") extends Keyword(literal)

  case class Return(override val literal: String = "RETURN") extends Keyword(literal)

  case class Rem(override val literal: String = "REM") extends Keyword(literal)

  case class If(override val literal: String = "IF") extends Keyword(literal)

  case class Then(override val literal: String = "THEN") extends Keyword(literal)

  case class Fn(override val literal: String = "FN") extends Keyword(literal)

  case class Let(override val literal: String = "LET") extends Keyword(literal)

  case class Step(override val literal: String = "STEP") extends Keyword(literal)

  case class To(override val literal: String = "TO") extends Keyword(literal)

  case class Dim(override val literal: String = "DIM") extends Keyword(literal)

  case class End(override val literal: String = "END") extends Keyword(literal)

  case class Def(override val literal: String = "DEF") extends Keyword(literal)

  case class GoSub(override val literal: String = "GOSUB") extends Keyword(literal)

  // Other
  case class Delimiter(literal: String) extends BasicToken

  case class Identifier(literal: String) extends BasicToken

  case class Text(literal: String) extends BasicToken

  case class Number(literal: String) extends BasicToken

  case class LineNumber(literal: String) extends BasicToken

  // Operator
  trait Operator extends BasicToken
  case class Plus(literal: String = "+") extends Operator
  case class Minus(literal: String = "-") extends Operator
  case class Multiply(literal: String = "*") extends Operator
  case class Divide(literal: String = "/") extends Operator


  // Comparator
  class Comparator(val literal: String) extends BasicToken
  case class Equal() extends Comparator("=")
  case class Different() extends Comparator("!=")
  case class Greater() extends Comparator(">")
  case class Lesser() extends Comparator("<")
}
