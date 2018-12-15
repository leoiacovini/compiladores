package common

trait TokenKind
object TokenKind {
  case object Unknown extends TokenKind
}

case class Token(literal: String, kind: TokenKind = TokenKind.Unknown)
