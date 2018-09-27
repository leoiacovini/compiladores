import ConsumeFile.{ConsumeFileOutput, EndOfFile, FileLine}
import sun.reflect.generics.reflectiveObjects.NotImplementedException

object ConsumeLine {
  sealed trait ConsumeLineOutput // find better name
  case class AsciiChar(c: Char) extends ConsumeLineOutput //TODO: classificate character
  case object EndOfLine extends ConsumeLineOutput

  def apply(cfo: ConsumeFileOutput): Seq[ConsumeLineOutput] = {
    cfo match {
      case FileLine(line) => line.toSeq.map(c => AsciiChar(c)) :+ EndOfLine
      case EndOfFile => Seq.empty[ConsumeLineOutput]
      case _ => throw new NotImplementedException()
    }
  }
}
