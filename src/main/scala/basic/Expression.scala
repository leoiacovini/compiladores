package basic

import basic.Expression.{LeafNode, Node, OpNode}

object Expression {
  trait Node
  case class LeafNode(value: BasicToken) extends Node
  case class OpNode(operation: BasicToken.Operator, left: Node, right: Node) extends Node
}

case class Expression(items: BasicToken*) {

  private def toAST(ast: Node, exp: Seq[BasicToken]): (Node, Seq[BasicToken]) = {
    if (exp.isEmpty) {
      (ast, Seq())
    } else {
      val token = exp.head
      if (token.isInstanceOf[BasicToken.Identifier] || token.isInstanceOf[BasicToken.Number]) {
        toAST(LeafNode(token), exp.tail)
      } else if (token.isInstanceOf[BasicToken.OpenParenthesis]) {
        val result = toAST(ast, exp.tail)
        toAST(result._1, result._2)
      } else if (token.isInstanceOf[BasicToken.CloseParenthesis]) {
        (ast, exp.tail)
      } else if (token.isInstanceOf[BasicToken.Plus] || token.isInstanceOf[BasicToken.Minus]) {
        val right = toAST(ast, exp.tail)
        (OpNode(token.asInstanceOf[BasicToken.Operator], ast, right._1), right._2)
      } else if (token.isInstanceOf[BasicToken.Multiply] || token.isInstanceOf[BasicToken.Divide]) {
        val right = toAST(ast, exp.tail)
        (OpNode(token.asInstanceOf[BasicToken.Operator], ast, right._1), right._2)
      } else {
        (ast, Seq())
      }
    }
  }

  def toAST: Node = {
    toAST(null, items)._1
  }

}
