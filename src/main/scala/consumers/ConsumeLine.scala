package consumers

import automata.CharAlphabets
import consumers.ConsumeFile.{ConsumeFileOutput, EndOfFile, FileLine}
import sun.reflect.generics.reflectiveObjects.NotImplementedException

object ConsumeLine {
  sealed trait BasicAsciiFilter
  case object Useful extends BasicAsciiFilter
  case object Disposable extends BasicAsciiFilter
  case object Control extends BasicAsciiFilter

  private def getBasicAsciiFilter(c: Char) = {
    c match {
      case d if d.isControl => Control
      case w if w.isWhitespace => Disposable
      case l if l.isLetterOrDigit || CharAlphabets.Special.contains(l) => Useful
      case e => throw new Exception("Could not classifcate Basic Ascii Filter for " + e + " character")
    }
  }

  sealed trait AsciiCategory
  case object Letter extends AsciiCategory
  case object Delimiter extends AsciiCategory
  case object Digit extends AsciiCategory
  case object Special extends AsciiCategory

  sealed trait ConsumeLineOutput // find better name
  case class AsciiChar(c: Char, baf: BasicAsciiFilter, asciiCategory: AsciiCategory) extends ConsumeLineOutput
  case object EndOfLine extends ConsumeLineOutput

  private def getAsciiCategory(c: Char): AsciiCategory = {
    c match {
      case l if l.isLetter => Letter
      case d if d.isDigit => Digit
      case s if CharAlphabets.Special.contains(s) => Special
      case w if w.isWhitespace => Delimiter
    }
  }

  def apply(cfo: ConsumeFileOutput): Seq[ConsumeLineOutput] = {
    cfo match {
      case FileLine(line) => line.toSeq.foldLeft(Seq.empty[ConsumeLineOutput]) {(chars, c) =>
        val baf = getBasicAsciiFilter(c)
        val isLastCharDelimiter = chars.lastOption match {
          case Some(a: AsciiChar) => a.asciiCategory == Delimiter
          case _ => false
        }
        if(baf == Disposable && isLastCharDelimiter)
          chars
        else
          chars :+ AsciiChar(c, baf, getAsciiCategory(c))
      } :+ EndOfLine
      case EndOfFile => Seq.empty[ConsumeLineOutput]
      case _ => throw new NotImplementedException()
    }
  }
}
