package basic

import llvm.{LLVMGlobalString, LLVMProgram}

object BasicToLLVM {
  def addPrintText(llvmProgram: LLVMProgram, text: BasicToken.Text): LLVMProgram = {
    val localVariable = "str" + Math.random()
    val str = LLVMGlobalString(localVariable, text.literal)
    llvmProgram.copy(
      constants = llvmProgram.constants :+ str,
      statements = llvmProgram.statements :+ LLVMProgram.getGlobalStringPtr(str) :+ LLVMProgram.puts(s"${localVariable}_cast")
    )
  }

  def addPrint(llvmProgram: LLVMProgram, print: BasicCommand.Print): LLVMProgram = {
    print.items.foldLeft(llvmProgram) { case (llvm, exp) =>
      exp match {
        case Expression(t: BasicToken.Text) => addPrintText(llvm, t)
        case _ => ???
      }
    }
  }
}
