package basic

import llvm.LLVMProgram
import org.scalatest.WordSpec
import java.io.{File, FileOutputStream, OutputStream, OutputStreamWriter}

class BasicToLLVMTest extends WordSpec {

  "A test" in {
    val llvm = LLVMProgram.empty
    val llvm2 = BasicToLLVM.addPrint(llvm, DPrint("fooo"))
    val file = new File("teste.ll")
    val os = new FileOutputStream(file)
    os.write(llvm2.toString.getBytes)
    os.close()
    println(file.getAbsolutePath)
    println(llvm2.toString)
  }

}
