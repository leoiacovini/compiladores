package basic

import llvm.LLVMProgram
import org.scalatest.WordSpec
import java.io.{File, FileOutputStream, OutputStream, OutputStreamWriter}

class BasicToLLVMTest extends WordSpec {

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
    val file = new File("teste.ll")
    val os = new FileOutputStream(file)
    os.write(context3.llvm.toString.getBytes)
    os.close()
    println(file.getAbsolutePath)
    println(context3.llvm.toString)
  }

}
