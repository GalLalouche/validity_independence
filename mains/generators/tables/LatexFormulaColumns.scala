package mains.generators.tables

abstract class LatexFormulaColumns(_columnNames: Seq[String], formulas: Seq[Seq[Double] => Any]) extends (Seq[Double] => Seq[Any]) {
  val columnNames = _columnNames.toList
  override def apply(xs: Seq[Double]): Seq[Any] = formulas map (e => e.apply(xs))
  def split: (Seq[String], Seq[Seq[Double] => Any]) =
    (columnNames, formulas)
}