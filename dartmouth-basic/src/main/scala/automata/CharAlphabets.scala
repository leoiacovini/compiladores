package automata

object CharAlphabets {
  val AlphabeticUppercase: Set[Char] = ('A' to 'Z').toSet
  val AlphabeticLowercase: Set[Char] = ('a' to 'z').toSet
  val Numeric: Set[Char] = ('0' to '9').toSet
  val Alphabetic: Set[Char] = AlphabeticUppercase ++ AlphabeticLowercase
  val Alphanumeric: Set[Char] = Alphabetic ++ Numeric
  val Special: Set[Char] = Set('!', '@', '#', '%', '"', '&', '*', '(', ')', '_', '+', '-', '=', '§', '{', '[', 'a', '}',
    ']', 'o', '?', '/', '°', '`', '\'', '^', '~', '<', ',', '>', '.', ':', ';', '|', '\\', '“', '”')
}
