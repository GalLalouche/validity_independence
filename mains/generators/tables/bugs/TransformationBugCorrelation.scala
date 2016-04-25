package mains.generators.tables.bugs

import corpus.bugs.CorporaDataWithBugs
import mains.generators.transformators.CorpusTransformator
import mains.programs.{Configuration, MetricValuesParser}
import corpus.rich.RichCorporaData._

trait TransformationBugCorrelation[T] extends LatexBugCorrelationGenerator[T] {
	protected def t: CorpusTransformator
	override protected lazy val rawData =
		(t apply MetricValuesParser.getData.ignoreMetrics(Configuration.ignoredMetrics)).getAverageValuesPerFile
	override def apply(data: CorporaDataWithBugs[T]) { super.apply(t applyWithBugs data) }
}
