package basic

import automata.ndpa.RunHistoryItem
import llvm.{LLVMGlobalString, LLVMProgram}
import wirth.{Terminal, WirthLexicalToken, WirthToNDPA}

object BasicToLLVM {
  def getCommand(seq: Seq[RunHistoryItem[WirthToNDPA.WirthGeneratedState, WirthLexicalToken, WirthToNDPA.StackAlphabet]]): DartmouthBasicCommand = {
    val inputs = seq.filter(_.inputSymbolOpt.isDefined).map(_.inputSymbolOpt.get)
    inputs.head match {
      case Terminal("PRINT") =>
        val printArg = inputs.drop(2).dropRight(2).map {
        case Terminal(a) => a
        case _ => ""
      }.reduce(_ + _)
        Print(printArg)
    }
  }

  def addPrint(llvmProgram: LLVMProgram, print: Print): LLVMProgram = {
    val localVariable = "str" + Math.random()
    val str = LLVMGlobalString(localVariable, print.arg)
    llvmProgram.copy(
      constants = llvmProgram.constants :+ str,
      statements = llvmProgram.statements :+ LLVMProgram.getGlobalStringPtr(str) :+ LLVMProgram.puts(s"${localVariable}_cast")
    )
  }
}
trait DartmouthBasicCommand
case class Print(arg: String) extends DartmouthBasicCommand
case class OtherCommand()
