package llvm

import java.io.{File, FileOutputStream}

object OutputWriter {
  def write(llvm: LLVMProgram, filepath: String): Unit = {
    val file = new File(filepath)
    val os = new FileOutputStream(file)
    os.write(llvm.toString.getBytes)
    os.close()
  }
}
