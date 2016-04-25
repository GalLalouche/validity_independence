package parsers.git

import java.util.regex.Pattern

import common.rich.path.RichFile
import common.rich.primitives.RichString.richString
import parsers.CsvFormatter

/**
 * Specifically for the csv logs of git data, since comments can have commas and "" in them
 */
class LogsCsvParser extends CsvFormatter[Seq[String]](_.map(str ⇒ s""""$str""""), null,
	List("filename", "revisionid", "date", "author", "comment", "diffs")) {

	import parsers.git.LogsCsvParser._

	override def asSequence(path: String): Seq[Seq[String]] = {
		try {
			val lines = RichFile(path).lines
			val columnNames = lines(0).split(",")
			lines
				.zipWithIndex
				.drop(1)
				.filterNot(_._1.isEmpty)
				.map({
				case (line, i) ⇒
					toSeq(line, columnNames.last == "diffs", i).map(_.withoutTrailingQuotes)
			})
		} catch {
			case ie: IndexOutOfBoundsException ⇒
				println(s"CSV data file $path has wrong number of columns")
				throw ie
		}
	}
}

object LogsCsvParser {
	private val regexp = Pattern.compile( """"\d+(;\d+)*"""")

	private def toSeq(line: String, hasDiffs: Boolean, lineIndex: Int): Seq[String] = {
		try {
			val split = line.split(",").toVector
			val hasCommaSeparatedDate = split(2).length == 4
			val dateLength = if (hasCommaSeparatedDate) 2 else 1
			val $ = split.take(2) :+
				(if (hasCommaSeparatedDate) s"${split(2)},${split(3)}" else split(2)) :+
				split(2 + dateLength) :+
				split.drop(3 + dateLength).take(split.size - (3 + dateLength) - (if (hasDiffs) 1 else 0)).mkString(",") :+
				split.last
			if (hasDiffs) $ else $.take($.size - 1)
		} catch {
			case e: Exception ⇒ throw new Exception(s"caughed exception at index $lineIndex", e)
		}
	}
}