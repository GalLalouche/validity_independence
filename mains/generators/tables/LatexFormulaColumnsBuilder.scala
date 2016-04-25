package mains.generators.tables

import common.rich.collections.RichSeq._

class LatexFormulaColumnsBuilder private(columnNames: Seq[String], expandedColumnNames: Seq[String],
																				 formulas: Seq[Seq[Double] => Any]) {
	def this() = this(Vector(), Vector(), Vector())
	def prepend(name: String, formula: Seq[Double] => Any) =
		new LatexFormulaColumnsBuilder(name :: columnNames, name :: expandedColumnNames, formula :: formulas)
	def append(name: String, formula: Seq[Double] => Any) =
		new LatexFormulaColumnsBuilder(columnNames + name, expandedColumnNames + name, formulas + formula)
	def addMultiColumn(name: String, formulas: Seq[(String, Seq[Double] => Any)]) =
		new LatexFormulaColumnsBuilder(
			columnNames + s"\\multicolumn{${formulas.size}}{c}{$name}",
			expandedColumnNames ++ formulas.map(_._1),
			this.formulas ++ formulas.map(_._2))

	def build: LatexFormulaColumns = new LatexFormulaColumns(columnNames, formulas) {
		override def apply(xs: Seq[Double]) = formulas map (_(xs))
		override def split = (expandedColumnNames, super.split._2)
	}
}
