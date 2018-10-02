package consumers

import scala.io.Source

object ConsumeFile {

  trait ConsumeFileOutput // find better name
  case class FileLine(line: String) extends ConsumeFileOutput
  case object EndOfFile extends ConsumeFileOutput

  def apply(filepath: String): Seq[ConsumeFileOutput] = {
    val lines: List[ConsumeFileOutput] = for(line <- Source.fromFile(filepath).getLines().toList) yield FileLine(line)
    lines :+ EndOfFile
  }
}
