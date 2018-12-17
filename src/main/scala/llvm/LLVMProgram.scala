package llvm

import scala.util.Try

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

case class LLVMPrintDouble(localVariable: String) extends LLVMStatement {
  override def getStatement: String =
  s"""tail call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.floatFormatString, i32 0, i32 0), double %$localVariable)"""
}

case class LLVMCall(function: String, ty: String, args: Seq[TypedValue]) extends LLVMStatement {
  override def getStatement: String =
    s"""call $ty @$function(${args.map(_.getString).mkString(", ")})"""
}

case class LLVMStore(value: TypedValue, where: TypedValue) extends LLVMStatement {
  override def getStatement: String =
    s"""store ${value.getString}, ${where.getString}"""
}

case class LLVMConditionalBranch(conditional: String, ifTrue: String, ifFalse: String) extends LLVMStatement {
  override def getStatement: String =
    s"""br i1 %$conditional, label %$ifTrue, label %$ifFalse"""
}

case class LLVMUnconditionalBranch(label: String) extends LLVMStatement {
  override def getStatement: String =
    s"""br label %$label"""
}

case class LLVMLoad(result: TypedValue, from: TypedValue) extends LLVMStatement {
  override def getStatement: String =
    s"""%${result.name} = load ${result.typ}, ${from.getString}"""
}

case class LLVMAlloca(result: TypedValue) extends LLVMStatement {
  override def getStatement: String =
    s"""%${result.name} = alloca ${result.typ}"""
}

case class LLVMLabel(label: String) extends LLVMStatement {
  override def getStatement: String =
    s"\n$label:"
}

case class LLVMCompareEqual(result: String, typ: String, left: String, right: String) extends LLVMStatement {
  override def getStatement: String =
    s"""%$result = fcmp oeq $typ %$left, %$right"""
}

case class LLVMAssign(result: TypedValue, value: String) extends LLVMStatement {

  override def getStatement: String =
    s"""%${result.name} = fadd ${result.typ} 0.0, ${Try(value.toInt).map(i => s"$i.0").getOrElse(value)}"""
}

case class LLVMAddLocalVariables(result: String, left: String, right: String) extends LLVMStatement {
  override def getStatement: String = s"%$result = fadd double %$left, %$right"
}

case class LLVMSubtractLocalVariables(result: String, left: String, right: String) extends LLVMStatement {
  override def getStatement: String = s"%$result = fsub double %$left, %$right"
}

case class LLVMMultiplyLocalVariables(result: String, left: String, right: String) extends LLVMStatement {
  override def getStatement: String = s"%$result = fmul double %$left, %$right"
}

case class LLVMDivideLocalVariables(result: String, left: String, right: String) extends LLVMStatement {
  override def getStatement: String = s"%$result = fdiv double %$left, %$right"
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

  def addLocalVariables(left: String, right: String, tempCount: Int): LLVMAddLocalVariables = {
    LLVMAddLocalVariables(s"temp.$tempCount", left, right)
  }

  def subtractLocalVariables(left: String, right: String, tempCount: Int): LLVMSubtractLocalVariables = {
    LLVMSubtractLocalVariables(s"temp.$tempCount", left, right)
  }

  def multiplyLocalVariables(left: String, right: String, tempCount: Int): LLVMMultiplyLocalVariables = {
    LLVMMultiplyLocalVariables(s"temp.$tempCount", left, right)
  }

  def divideLocalVariables(left: String, right: String, tempCount: Int): LLVMDivideLocalVariables = {
    LLVMDivideLocalVariables(s"temp.$tempCount", left, right)
  }

  def storeVariable(index: Int, value: Int, tempCount: Int): Seq[LLVMStatement] = {
    Seq(
      LLVMGetElementPtr2(
        s"temp.$tempCount",
        "double",
        TypedValue("double*", "%variables"),
        TypedValue("i32", index.toString)
      ),
      LLVMStore(
        TypedValue("double", value.toString + ".0"),
        TypedValue("double*", s"%temp.$tempCount")
      )
    )
  }

  def printTemp(tempCount: Int): Seq[LLVMStatement] = {
    Seq(
      LLVMPrintDouble(s"temp.$tempCount")
    )
  }

  def storeConstant(value: Int, tempCount: Int): Seq[LLVMStatement] = {
    Seq(
      LLVMAssign(
        TypedValue("double", s"temp.$tempCount"),
        value.toString
      )
    )
  }

  def label(lineNumber: Int): Seq[LLVMStatement] = {
    Seq(
      LLVMUnconditionalBranch(s"line$lineNumber"),
      LLVMLabel(s"line$lineNumber")
    )
  }

  def ifEqualsThenLabel(tempLeft: Int, tempRight: Int, lineNumberThen: Int, tempCount: Int): Seq[LLVMStatement] = {
    Seq(
      LLVMCompareEqual(s"temp.$tempCount", "double", s"temp.$tempLeft", s"temp.$tempRight"),
      LLVMConditionalBranch(s"temp.$tempCount", s"line$lineNumberThen", s"deadelse$tempCount"),
      LLVMLabel(s"deadelse$tempCount")
    )
  }

  def loadVariable(variableIndex: Int, tempCount: Int): Seq[LLVMStatement] = {
    Seq(
      LLVMGetElementPtr2(
        s"temp.${tempCount}_ptr",
        "double",
        TypedValue("double*", "%variables"),
        TypedValue("i32", variableIndex.toString)
      ),
      LLVMLoad(
        TypedValue("double", "temp." + tempCount),
        TypedValue("double*", s"%temp.${tempCount}_ptr")
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
    "declare void @free(i8*)",
    "declare i32 @printf(i8*, ...)",
    "@.integerFormatString = internal constant [4 x i8] c\"%d\\0A\\00\"",
    "@.floatFormatString = internal constant [4 x i8] c\"%f\\0A\\00\""

  )

  override def toString: String = {
    s"""
       |${constants.map(_.getDeclaration).mkString("\n")}
       |
       |${externalDeclarations.mkString("\n")}
       |
       |define i32 @main() {
       |    %variables = alloca double, i32 100
       |
       |    ${statements.map(_.getStatement).mkString("\n    ")}
       |
       |
       |    ret i32 0
       |}
    """.stripMargin
  }
}
