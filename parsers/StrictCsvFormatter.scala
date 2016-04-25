package parsers

import java.io.IOException

import common.rich.collections.RichIterator._
import common.rich.path.RichFile

/** Same as CsvFormatter, but calls to asSequence may throw an exception if file is not in the expected format.
  * Specifically, the column names will be checked, and all columns have to be the same length
  */
class StrictCsvFormatter[T](toCsv: T ⇒ Seq[String], fromCsv: Seq[String] ⇒ T, columnNames: Seq[String])
	extends CsvFormatter[T](toCsv, fromCsv, columnNames) {

	override def asSequence(path: String): Seq[T] = {
		val lines = RichFile(path)
			.lines
		val actualColumnNames = lines(0).trim.split("\\s?,\\s?").toList
		if (columnNames.toList != actualColumnNames)
			throw new IOException(s"expected column names ($columnNames) are different than existing ($actualColumnNames) for file $path")
		lines.iterator
			.drop(1) // dropping column names
			.map(_ split ",")
			.verify(_.length == columnNames.size, (e, i) => s"${e.toSeq} @ $i @$path does not have enough columns (was ${e.size} and expected ${columnNames.size})")
			.map(fromCsv(_))
			.toVector
	}
}
