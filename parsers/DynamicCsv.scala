package parsers

import java.io.File

import common.rich.path.RichFile.richFile
import common.rich.primitives.RichString.richString
import common.rich.collections.RichTraversable._
/**
 * This class loads a dynamic CSV file (i.e., untyped) and provides convenience methods
 * for accessing it.
 * Other than its dynamic nature, it differs from CsvFormatter in that data is treated as columned,
 * rather than a type for every row.
 */
class DynamicCsv private(data: Map[String, Seq[String]]) extends (String => Seq[String]) {
  require(data.nonEmpty)

  def this(data: Seq[(String, Seq[String])]) = this(data.toMap)

  /**
   * Gets the data associated with the column named str. some entries might be empty (i.e., "")
   */
  override def apply(str: String): Seq[String] = data(str)

  /**
   * Gets all data for columns named c1 and c2. only entries which have value (i.e., non-empty string)
   * for BOTH columns will be returned.
   */
  def zip(c1: String, c2: String): Seq[(String, String)] = apply(c1)
    .zip(apply(c2))
    .filterNot(e => e._1.isEmpty || e._2.isEmpty)
}

object DynamicCsv {
  def load(file: File) = {
    val lines = file.lines
    require(lines.size > 0, s"CSV file $file is empty")
    require(lines.size > 1, s"CSV file $file only has headers")
    val columns: Seq[String] = lines(0).split(",")
    require(columns.allUnique, s"CSV file $file has repeating header names: $columns")
    val data: Seq[Seq[String]] = lines
      .drop(1)
      .map(_.smartSplit(','))
      .transpose
    new DynamicCsv(columns zip data)
  }
}