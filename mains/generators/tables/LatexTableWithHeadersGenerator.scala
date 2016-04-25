package mains.generators.tables

import java.io.File

import common.rich.path.RichFile
import common.rich.path.RichFile._
import common.rich.RichT._


import scala.collection.mutable.ListBuffer

//TODO Find a way to make this immutable
abstract class LatexTableWithHeadersGenerator extends LatexTableGenerator {
	protected def headers: Seq[String]

	protected val body: ListBuffer[Any] = ListBuffer()
	protected def setup(file: java.io.File) {
		// less log spam
		unclearedFile.clear
		appendTopRule
		file.appendLine(headers.toLatexLine)
		appendMidRule
		appendBody(file)
		appendBottomRule
	}

	def appendBody(file: RichFile) {
		body.map {
			case xs: Seq[Any] => xs.toLatexLine
			case str: String => str
		}.foreach(file.appendLine)
	}
	def appendLine(str: String) {
		body += str
		setup(file)
	}

	override def writeLineToLatex(line: Seq[Any], file: java.io.File) {
		body += line
		setup(file)
	}
	override def writeLinesToLatex(xss: Seq[Seq[Any]], file: File) {
		// slightly more efficient
		//		for (h <- headers; line <- xss)
		//		require(h.size == line.size, s"Line's length does not match headers... Headers: $h, line: $line, sizes: ${h.size} vs ${line.size}")
		body ++= xss
		setup(file)
		println(file.readAll)
	}
}
