abstract class Token(literal: String)

case class IdentifierToken(literal: String) extends Token(literal)
case class OperatorToken(literal: String) extends Token(literal)
case class ValueToken(literal: String) extends Token(literal)
case class ReservedWordToken(literal: String) extends Token(literal)
case class UnknownToken(literal: String) extends Token(literal)

