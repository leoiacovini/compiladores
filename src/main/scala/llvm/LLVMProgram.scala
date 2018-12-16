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

case class LLVMGetElementPtr2(result: String, typ: String, from: TypedValue, index: TypedValue) extends LLVMStatement {
  override def getStatement: String = s"%$result = getelementptr $typ, ${from.getString}, ${index.getString}"
}

case class LLVMCall(function: String, ty: String, args: Seq[TypedValue]) extends LLVMStatement {
  override def getStatement: String =
    s"""call $ty @$function(${args.map(_.getString).mkString(", ")})"""
}
case class LLVMStore(value: TypedValue, where: TypedValue) extends LLVMStatement {
  override def getStatement: String =
    s"""store ${value.getString}, ${where.getString}"""
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

  def storeVariable(index: Int, value: Int, tempCount: Int): Seq[LLVMStatement] = {
    Seq(
      LLVMGetElementPtr2(
        "variable_ptr." + tempCount,
        "i32",
        TypedValue("i32*", "%variables"),
        TypedValue("i32", index.toString)
      ),
      LLVMStore(
        TypedValue("i32", value.toString),
        TypedValue("i32*", "%variable_ptr." + tempCount)
      )
    )
  }

  def puts(local: String): LLVMCall = {
    LLVMCall("puts", "i32", Seq(TypedValue("i8*", s"%$local")))
  }

  def empty: LLVMProgram = new LLVMProgram(Seq.empty, Seq.empty)

}

case class LLVMProgram(constants: Seq[LLVMGlobalConstant[_]], statements: Seq[LLVMStatement]) {
  def addStatement(statement: LLVMStatement): LLVMProgram = copy(statements = statements :+ statement)
  val putsExternalDeclaration = "declare i32 @puts(i8* nocapture) nounwind"
  val externalDeclarations = Seq(
    "declare i32 @puts(i8* nocapture) nounwind",
    "declare i8* @calloc(i32, i32)",
    "declare void @free(i8*)"
  )
  override def toString: String = {
    s"""
       |${constants.map(_.getDeclaration).mkString("\n")}
       |
       |${externalDeclarations.mkString("\n")}
       |
       |define i32 @main() {
       |    %variables = alloca i32, i32 100
       |    %cells = call i8* @calloc(i32 30000, i32 1)
       |    %cell_index_ptr = alloca i32
       |    %variable_index_ptr = alloca i32
       |
       |    store i32 0, i32* %cell_index_ptr
       |    store i32 0, i32* %variable_index_ptr
       |    ${statements.map(_.getStatement).mkString("\n    ")}
       |    call void @free(i8* %cells)
       |
       |    %variable_ptr2 = getelementptr i32, i32* %variables, i32 1
       |    %variable2 = load i32, i32* %variable_ptr2
       |    ret i32 %variable2
       |}
    """.stripMargin
  }
}
