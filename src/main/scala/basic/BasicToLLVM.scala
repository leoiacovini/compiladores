package basic

import basic.BasicToLLVM.{SymbolTable, VariableInformation}
import basic.BasicToken._
import basic.Expression.{LeafNode, OpNode}
import llvm.{LLVMAddLocalVariables, LLVMGlobalString, LLVMProgram, LLVMStatement}

import scala.util.Try

case class CodeGenerationContext(llvm: LLVMProgram,
                                 symbolTable: SymbolTable,
                                 currentIndex: Int = 0,
                                 tempCount: Int = 0) {
  def withLLVM(llvm: LLVMProgram): CodeGenerationContext = copy(llvm = llvm)
  def addStatements(statements: Seq[LLVMStatement]): CodeGenerationContext = withLLVM(statements.foldLeft(llvm) {
    (llvmProgram, statement) => llvmProgram.addStatement(statement)}
  )
  def incrementTempCount: CodeGenerationContext = copy(tempCount = tempCount + 1)
  def withVariable(variableName: BasicToken.Identifier): CodeGenerationContext = copy(
    symbolTable = symbolTable + (variableName -> VariableInformation(currentIndex)),
    currentIndex = currentIndex + 1
  )

  def getVariableIndex(variableName: BasicToken.Identifier): Int = symbolTable(variableName).index
}
object BasicToLLVM {

  case class VariableInformation(index: Int)
  type SymbolTable = Map[BasicToken.Identifier, VariableInformation]
  private def addPrintText(llvmProgram: LLVMProgram, text: BasicToken.Text, tempCount: Int): LLVMProgram = {
    val localVariable = "str" + tempCount
    val str = LLVMGlobalString(localVariable, text.literal)
    llvmProgram.copy(
      constants = llvmProgram.constants :+ str,
      statements = llvmProgram.statements :+ LLVMProgram.getGlobalStringPtr(str) :+ LLVMProgram.puts(s"${localVariable}_cast")
    )
  }

  def addPrint(context: CodeGenerationContext, print: BasicCommand.Print): CodeGenerationContext = {
    print.items.foldLeft(context) { case (ctx, exp) =>
      exp match {
        case Expression(t: BasicToken.Text) => ctx.withLLVM(addPrintText(context.llvm, t, context.tempCount)).incrementTempCount
        case _ => ???
      }
    }
  }

  def addAssign(context: CodeGenerationContext, assign: BasicCommand.Assign): CodeGenerationContext = {
    assign.exp match {
      case Expression(i: BasicToken.Number) if Try(i.literal.toInt).isSuccess =>
        context
          .withVariable(assign.varName)
          .addStatements(LLVMProgram.storeVariable(context.currentIndex, i.literal.toInt, context.tempCount))
          .incrementTempCount
      case _ => ???
    }
  }

  def calcNode(context: CodeGenerationContext, node: Expression.Node): CodeGenerationContext = {
    node match {
      case LeafNode(value) =>
        value match {
          case id: Identifier =>
            val variableIndex = context.getVariableIndex(id)
            val load = LLVMProgram.loadVariable(variableIndex, context.tempCount)
            println(s"temp${context.tempCount} = ${id.literal}")
            context.addStatements(load).incrementTempCount
          case Text(literal) => throw new Exception(s"Could not calculate expression for text literal '$literal'")
          case Number(literal) if Try(literal.toInt).isSuccess =>
            println(s"temp${context.tempCount} = $literal")
            context.addStatements(LLVMProgram.storeConstant(literal.toInt, context.tempCount)).incrementTempCount
          case _ => throw new Exception(s"Could not generate expression for token")
        }
      case OpNode(operation, left, right) =>
        operation match {
          case Plus(_) =>
            val contextAfterLeft = calcNode(context, left)
            val contextAfterRight = calcNode(contextAfterLeft, right)
            val leftIndex = contextAfterLeft.tempCount - 1
            val rightIndex = contextAfterRight.tempCount - 1
            println(s"temp${contextAfterRight.tempCount} = temp$leftIndex + temp$rightIndex")

            contextAfterRight.addStatements(
              Seq(
                LLVMProgram.addLocalVariables(
                  s"temp.$leftIndex",
                  s"temp.$rightIndex",
                  contextAfterRight.tempCount
                )
              )
            ).incrementTempCount
          case Minus(_) =>
            val contextAfterLeft = calcNode(context, left)
            val contextAfterRight = calcNode(contextAfterLeft, right)
            val leftIndex = contextAfterLeft.tempCount - 1
            val rightIndex = contextAfterRight.tempCount - 1
            println(s"temp${contextAfterRight.tempCount} = temp$leftIndex - temp$rightIndex")

            contextAfterRight.addStatements(
              Seq(
                LLVMProgram.subtractLocalVariables(
                  s"temp.$leftIndex",
                  s"temp.$rightIndex",
                  contextAfterRight.tempCount
                )
              )
            ).incrementTempCount
          case Multiply(_) =>
            val contextAfterLeft = calcNode(context, left)
            val contextAfterRight = calcNode(contextAfterLeft, right)
            val leftIndex = contextAfterLeft.tempCount - 1
            val rightIndex = contextAfterRight.tempCount - 1
            println(s"temp${contextAfterRight.tempCount} = temp$leftIndex * temp$rightIndex")

            contextAfterRight.addStatements(
              Seq(
                LLVMProgram.multiplyLocalVariables(
                  s"temp.$leftIndex",
                  s"temp.$rightIndex",
                  contextAfterRight.tempCount
                )
              )
            ).incrementTempCount
          case Divide(_) =>
            val contextAfterLeft = calcNode(context, left)
            val contextAfterRight = calcNode(contextAfterLeft, right)
            val leftIndex = contextAfterLeft.tempCount - 1
            val rightIndex = contextAfterRight.tempCount - 1
            println(s"temp${contextAfterRight.tempCount} = temp$leftIndex / temp$rightIndex")

            contextAfterRight.addStatements(
              Seq(
                LLVMProgram.divideLocalVariables(
                  s"temp.$leftIndex",
                  s"temp.$rightIndex",
                  contextAfterRight.tempCount
                )
              )
            ).incrementTempCount
          case _ => ???
        }
      case _ => ???
    }
  }
  def calcExpression(context: CodeGenerationContext, expression: Expression): CodeGenerationContext = {
    val t: Expression.Node = expression.toAST
    calcNode(context, t)
  }
}
