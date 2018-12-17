package basic

import llvm.LLVMProgram
import org.scalatest.WordSpec
import java.io.{File, FileOutputStream, OutputStream, OutputStreamWriter}

class BasicToLLVMTest extends WordSpec {
  def writeTestFile(context: CodeGenerationContext): Unit = {
    val file = new File("teste.ll")
    val os = new FileOutputStream(file)
    os.write(context.llvm.toString.getBytes)
    os.close()
    println(file.getAbsolutePath)
    println(context.llvm.toString)
  }
  "Another test" in {
    val llvm = LLVMProgram.empty
    val context = CodeGenerationContext(llvm, Map.empty)
    val context2 = BasicToLLVM.addAssign(
      context,
      BasicCommand.Assign(BasicToken.Identifier("A"), Expression(BasicToken.Number("34")))
    )
    val context3 = BasicToLLVM.addAssign(
      context2,
      BasicCommand.Assign(BasicToken.Identifier("B"), Expression(BasicToken.Number("42")))
    )
    val context4 = BasicToLLVM.addLineNumber(context3, BasicToken.LineNumber("10"))
    val expression = Expression(
      BasicToken.OpenParenthesis(),
      BasicToken.Number("10"),
      BasicToken.Plus(),
      BasicToken.Number("20"),
      BasicToken.Plus(),
      BasicToken.Identifier("A"),
      BasicToken.CloseParenthesis(),
      BasicToken.Multiply(),
      BasicToken.Number("5"))
    val context5 = BasicToLLVM.calcExpression(context4, expression)
    println(context5.symbolTable)
    context5.llvm.statements.foreach(println)
    writeTestFile(context5.addStatements(LLVMProgram.printTemp(8)))
  }



}
