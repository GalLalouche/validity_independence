package parsers

import java.io.File

import common.rich.path.RichFile.richFile

object CsvParser {
  def parse[T](f: Seq[String] ⇒ T, file: File): Seq[T] = {
    file.lines.to[Vector].drop(1).map(x ⇒ f(x.split(",|\t")))
  }
}