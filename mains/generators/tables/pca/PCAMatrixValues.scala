package mains.generators.tables.pca

import common.rich.RichAll._
import corpus.CorporaData
import corpus.rich.RichData._
import mains.generators.tables.{LatexFormulaColumnsBuilder, LatexTableWithDynamicHeaders}
import mains.generators.transformators.LatexNamedTransformator
import mains.programs.Debug
import stats.PCAMetric

private[tables] abstract class PCAMatrixValues
	extends LatexTableWithDynamicHeaders with Debug {
	protected def extractDataForNormalization(data: CorporaData): Traversable[Traversable[Double]]

	def apply(t: (CorporaData, Seq[LatexNamedTransformator])) {
		val (data, normalizations) = t
		val headers = "metric" ::
			normalizations.foldLeft(new LatexFormulaColumnsBuilder())(// this is generated only for the names
				(builder, normalization) =>
					builder.addMultiColumn(normalization.toString, Seq.fill(2)(("text doesn't matter", e => e))))
				.build.columnNames
		val metricNames = PCAMetric.generateMetrics(data.metrics.size)
		val body = for ((eigenValues, index) <- normalizations
			.map(_ apply data)
			.map(extractDataForNormalization)
			.transpose.zipWithIndex)
		yield metricNames(index) :: eigenValues.flatMap(e =>
				Seq(e.mean, (100 * e.standardDeviation / e.mean).toInt.toString)
			)
		writeTable(headers, body)
	}

	def apply(data: CorporaData, normalizations: Seq[LatexNamedTransformator]) { apply((data, normalizations)) }

	def withName(f: String => String) = new PCAMatrixValues {
		override protected def extractDataForNormalization(data: CorporaData) =
			PCAMatrixValues.this.extractDataForNormalization(data)
		override val fileName = f(PCAMatrixValues.this.fileName)
	}
}
