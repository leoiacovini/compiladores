package llvm

trait LLVMGlobalConstant[T] {
  def getDeclaration: String
}

case class LLVMGlobalString(name: String, value: String) extends LLVMGlobalConstant[String] {
  val valueLine = s"$value\\00"

  override def getDeclaration: String = s"""@.$name = private unnamed_addr constant [${value.length + 1} x i8] c"$valueLine""""
}

case class TypedValue(typ: String, name: String) {
  def getString = s"$typ $name"
}

trait LLVMStatement {
  def getStatement: String
}

case class LLVMGetElementPtr(result: String, ty: String, ptrval: String, indexes: Seq[TypedValue]) extends LLVMStatement {
  override def getStatement: String = s"%$result = getelementptr $ty, $ty* $ptrval, ${indexes.map(_.getString).mkString(", ")}"
}

case class LLVMCall(function: String, ty: String, args: Seq[TypedValue]) extends LLVMStatement {
  override def getStatement: String =
    s"""call $ty @$function(${args.map(_.getString).mkString(", ")})"""
}

object LLVMProgram {
  def getGlobalStringPtr(llvmGlobalString: LLVMGlobalString): LLVMGetElementPtr = {
    LLVMGetElementPtr(
      s"${llvmGlobalString.name}_cast",
      s"[${llvmGlobalString.value.length + 1} x i8]",
      s"@.${llvmGlobalString.name}",
      Seq(TypedValue("i64", "0"), TypedValue("i64", "0"))
    )
  }

  def puts(local: String): LLVMCall = {
    LLVMCall("puts", "i32", Seq(TypedValue("i8*", s"%$local")))
  }

  def empty: LLVMProgram = new LLVMProgram(Seq.empty, Seq.empty)

}

case class LLVMProgram(constants: Seq[LLVMGlobalConstant[_]], statements: Seq[LLVMStatement]) {
  val putsExternalDeclaration = "declare i32 @puts(i8* nocapture) nounwind"

  override def toString: String = {
    s"""
       |${constants.map(_.getDeclaration).mkString("\n")}
       |
       |$putsExternalDeclaration
       |
       |define i32 @main() {
       |    ${statements.map(_.getStatement).mkString("\n    ")}
       |    ret i32 0
       |}
    """.stripMargin
  }
}
