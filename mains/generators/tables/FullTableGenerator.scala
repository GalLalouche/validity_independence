package mains.generators.tables

import common.rich.RichT._
import common.rich.path.RichFile._
import common.rich.path.{IndentableRichFile, RichFile}

private[tables] abstract class FullTableGenerator extends LatexTableWithHeadersGenerator {
	private var indentFile = new IndentableRichFile(super.file)

	override def file: RichFile = indentFile

	protected val caption: String
	protected val label: String = fileName
	protected val alignments: String
	protected val preTabular: Seq[String] = Seq()

	/**
	* Gets a generator without all the table preamble
	*/
	def bodyOnly = new LatexTableGenerator {
		override def file: RichFile = FullTableGenerator.this.file
		override protected val formatTableItem: PartialFunction[Any, Any] = FullTableGenerator.this.formatTableItem
	}

	override protected def setup(file: java.io.File) {
		// less log spam
		indentFile = new IndentableRichFile(file)
			.clear
			.appendLine("\\begin{table}")
			.increaseIndent
			.mapTo { f => preTabular.foreach(f.appendLine); f} // append pretabular while keeping the expression
			.appendLine(s"\\begin{tabular}{$alignments}")
			.increaseIndent
		appendTopRule
		indentFile.appendLine(headers.toLatexLine)
		appendMidRule
		appendBody(indentFile)
		appendBottomRule
		indentFile
			.decreaseIndent
			.appendLine("\\end{tabular}")
			.appendLine(s"\\caption{$caption}")
			.appendLine(s"\\label{Table:$label}")
			.decreaseIndent
			.appendLine {"\\end{table}"}
			.reset
	}
}
