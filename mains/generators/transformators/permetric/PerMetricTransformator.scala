package mains.generators.transformators.permetric

import mains.generators.transformators.CorporaTransformator
import corpus._
import corpus.rich.RichData._

trait PerMetricTransformator extends CorporaTransformator {
	protected def applyForSingularMetric(data: CorporaData): CorporaData
	def apply(data: CorporaData): CorporaData = {
		val metrics = data.metrics
		(for (m <- metrics) yield applyForSingularMetric(data useMetrics Set(m)))
			.reduce(
				(current, next) => new CorporaData(current.corpora.zip(next.corpora)
					.map(e => new CorpusData(e._1.c, e._1.metricValues ++ e._2.metricValues))))
	}
}