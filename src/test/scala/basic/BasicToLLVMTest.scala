package basic

import llvm.LLVMProgram
import org.scalatest.WordSpec
import java.io.{File, FileOutputStream, OutputStream, OutputStreamWriter}

class BasicToLLVMTest extends WordSpec {
  def writeTestFile(llvm: LLVMProgram): Unit = {
    val file = new File("teste.ll")
    val os = new FileOutputStream(file)
    os.write(llvm.toString.getBytes)
    os.close()
    println(file.getAbsolutePath)
    println(llvm.toString)
  }
  "Another test" in {
    val assignA = BasicCommand.Assign(BasicToken.Identifier("A"), Expression(BasicToken.Number("43")))
    val assignB = BasicCommand.Assign(BasicToken.Identifier("B"), Expression(BasicToken.Number("42")))
    val ifCommand = BasicCommand.If(Expression(BasicToken.Identifier("A")), Expression(BasicToken.Identifier("B")), BasicToken.Equal(), BasicToken.Number("50"))
    val printElse = BasicCommand.Print(Seq(Expression(BasicToken.Text("ELSE"))))
    val printTrue = BasicCommand.Print(Seq(Expression(BasicToken.Text("TRUE"))))

    val basicProgram = Seq(
      BasicStatement(10, assignA),
      BasicStatement(20, assignB),
      BasicStatement(30, ifCommand),
      BasicStatement(40, printElse),
      BasicStatement(50, printTrue)
    )
    writeTestFile(BasicToLLVM.toLLVM(basicProgram))
  }



}
