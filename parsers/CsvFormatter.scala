package parsers

import java.io.File

import common.rich.path.RichFile
import common.rich.path.RichFile.richFile

import scala.Option.option2Iterable

class CsvFormatter[T] private (toCsv: T ⇒ Seq[String], fromCsv: Seq[String] ⇒ T, protected val columnNames: Option[Seq[String]]) {
  def this(toCsv: T ⇒ Seq[String], fromCsv: Seq[String] ⇒ T) = this(toCsv, fromCsv, None)
  def this(toCsv: T ⇒ Seq[String], fromCsv: Seq[String] ⇒ T, columnNames: Seq[String]) =
    this(toCsv, fromCsv, if (columnNames == null) None else Some(columnNames))

  def save(path: String)(data: Seq[T]) {
    val file = new File(path)
    file.createNewFile
    file.clear
    file.write((columnNames.map(_ mkString ",").map(Vector(_)).getOrElse(Nil) ++ data.map(toCsv(_) mkString ",")).mkString("\n"))
  }

  def save(f: File)(data: Seq[T]) {
    save(f.getAbsolutePath)(data)
  }

  def saver(path: String): T => Unit = {
    val file = new File(path)
    if(file.exists)
      file.delete()
    file.createNewFile()
    file.write(columnNames.map(_.mkString(",") + "\n").getOrElse(""))

    line => file.write((toCsv(line) mkString ",") + '\n')
  }

  def saver(f: File): T => Unit =
    saver(f.getAbsolutePath)

  def asSequence(path: String): Seq[T] = {
    RichFile(path)
      .lines
      .drop(columnNames.size)
      .map(line ⇒ fromCsv(line split ","))
  }

  def asSequence(f: File): Seq[T] = asSequence(f.getAbsolutePath)

  def withHeaders(headers: Seq[String]): CsvFormatter[T] = new CsvFormatter[T](toCsv, fromCsv, headers)
}
