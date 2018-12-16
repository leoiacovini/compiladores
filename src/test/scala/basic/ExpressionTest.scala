package basic

import basic.Expression.{LeafNode, OpNode}
import org.scalatest.WordSpec

class ExpressionTest extends WordSpec {

  "Creating AST" in {

    val expression = Expression(
        BasicToken.OpenParenthesis(),
        BasicToken.Number("10"),
        BasicToken.Plus(),
        BasicToken.Number("20"),
        BasicToken.Divide(),
        BasicToken.Identifier("X"),
        BasicToken.CloseParenthesis(),
        BasicToken.Multiply(),
        BasicToken.Number("5"))

    assert(expression.toAST ==
      OpNode(BasicToken.Multiply(),
        OpNode(BasicToken.Plus(), LeafNode(BasicToken.Number("10")),
          OpNode(BasicToken.Divide(), LeafNode(BasicToken.Number("20")), LeafNode(BasicToken.Identifier("X")))),
        LeafNode(BasicToken.Number("5"))))

  }

}
