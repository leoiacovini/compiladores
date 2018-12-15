package basic

abstract case class BasicToken(literal: String)

// Keywords
case class Keyword(override val literal: String) extends BasicToken(literal)
case class Read(override val literal: String) extends Keyword(literal)
case class Data(override val literal: String) extends Keyword(literal)
case class Print(override val literal: String) extends Keyword(literal)
case class Goto(override val literal: String) extends Keyword(literal)
case class For(override val literal: String) extends Keyword(literal)
case class Next(override val literal: String) extends Keyword(literal)
case class Return(override val literal: String) extends Keyword(literal)
case class Rem(override val literal: String) extends Keyword(literal)
case class Then(override val literal: String) extends Keyword(literal)
case class Fn(override val literal: String) extends Keyword(literal)
case class Let(override val literal: String) extends Keyword(literal)
case class Step(override val literal: String) extends Keyword(literal)
case class To(override val literal: String) extends Keyword(literal)
case class Dim(override val literal: String) extends Keyword(literal)
case class End(override val literal: String) extends Keyword(literal)
case class Def(override val literal: String) extends Keyword(literal)
case class GoSub(override val literal: String) extends Keyword(literal)

// Others
case class Operator(override val literal: String) extends BasicToken(literal)
case class Delimiter(override val literal: String) extends BasicToken(literal)
case class Identifier(override val literal: String) extends BasicToken(literal)
case class Number(override val literal: String) extends BasicToken(literal)