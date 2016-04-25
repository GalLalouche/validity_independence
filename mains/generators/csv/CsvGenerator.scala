package mains.generators.csv

import mains.generators.FileGenerator
import mains.programs.Configuration
import parsers.DynamicCsv

abstract class CsvGenerator(val headers: Seq[String], name: Option[String] = None) extends FileGenerator(name, "csv") {

	protected def appendLine(data: Seq[Any]) = {
		require(data.size == headers.size, "mismatched length")
		file.appendLine(data mkString ",")
		this
	}

	override lazy val file = unclearedFile.clear.appendLine(headers mkString ",")
}

object CsvGenerator {
	def defaultDirectory = Configuration.generated.addSubDir("Csv")
}
