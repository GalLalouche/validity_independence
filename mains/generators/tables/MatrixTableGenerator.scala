package mains.generators.tables

import common.rich.RichAll._

abstract class MatrixTableGenerator[Column, Row] extends FullTableGenerator {
	def createCell(t: Column, s: Row): Any
	private class Aux(columns: Seq[Column], rows: Seq[Row], rowsHeader: String) extends FullTableGenerator {

		override protected val preTabular: Seq[String] = MatrixTableGenerator.this.preTabular
		override protected val headers: Seq[String] =
			rowsHeader :: customHeaders(columns)
		def apply() {
			writeLinesToLatex(for (r <- rows) yield {
				r :: columns.map(createCell(_, r))
			})
		}
		override protected val caption: String = MatrixTableGenerator.this.caption
		override protected val alignments: String = MatrixTableGenerator.this.alignments

		override protected val formatTableItem: PartialFunction[Any, Any] = MatrixTableGenerator.this.formatTableItem
		override def fileName: String = MatrixTableGenerator.this.fileName
	}
	def apply(columns: Seq[Column], rows: Seq[Row])(implicit y: Manifest[Row]) {
		new Aux(columns, rows, y.runtimeClass.getSimpleName).apply()
	}

	override protected def headers: Seq[String] = ???

	//override this to provide your own headers!
	def customHeaders(columns: Seq[Column]): Seq[String] = columns.map(_.toString)

	def transpose: MatrixTableGenerator[Row, Column] = ???
//	{
//		new MatrixTableGenerator[Y, X]() {
//			override def createCell(t: Y, s: X): Any = MatrixTableGenerator.this.createCell(s, t)
//			override protected val caption: String = MatrixTableGenerator.this.caption
//			override protected val alignments: String = MatrixTableGenerator.this.alignments
//			override protected val headers: Seq[String] = MatrixTableGenerator.this.headers
//		}
//	}
}
