package basic

trait BasicToken {
  val literal: String
}

object BasicToken {

  // Keywords
  case class Keyword(literal: String) extends BasicToken(literal)

  case class Read(override val literal: String = "READ") extends Keyword(literal)

  case class Data(override val literal: String = "DATA") extends Keyword(literal)

  case class Print(override val literal: String = "PRINT") extends Keyword(literal)

  case class Goto(override val literal: String = "GOTO") extends Keyword(literal)

  case class For(override val literal: String = "FOR") extends Keyword(literal)

  case class Next(override val literal: String = "NEXT") extends Keyword(literal)

  case class Return(override val literal: String = "RETURN") extends Keyword(literal)

  case class Rem(override val literal: String = "REM") extends Keyword(literal)

  case class Then(override val literal: String = "THEN") extends Keyword(literal)

  case class Fn(override val literal: String = "FN") extends Keyword(literal)

  case class Let(override val literal: String = "LET") extends Keyword(literal)

  case class Step(override val literal: String = "STEP") extends Keyword(literal)

  case class To(override val literal: String = "TO") extends Keyword(literal)

  case class Dim(override val literal: String = "DIM") extends Keyword(literal)

  case class End(override val literal: String = "END") extends Keyword(literal)

  case class Def(override val literal: String = "DEF") extends Keyword(literal)

  case class GoSub(override val literal: String = "GOSUB") extends Keyword(literal)

  // Others
  case class Operator(literal: String) extends BasicToken(literal)

  case class Delimiter(literal: String) extends BasicToken(literal)

  case class Identifier(literal: String) extends BasicToken(literal)

  case class Number(literal: String) extends BasicToken(literal)

}
