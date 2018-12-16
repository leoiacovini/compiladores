package basic

import basic.BasicToLLVM.{SymbolTable, VariableInformation}
import llvm.{LLVMGlobalString, LLVMProgram, LLVMStatement}

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
}
