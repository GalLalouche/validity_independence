package mains.generators.tables.bugs

import common.TupleFunction._
import corpus.bugs.MetricDataWithBugs
import stats.Correlation

class NumericalBugCorrelation(c: Correlation) extends LatexBugCorrelationGenerator[Double] {
	require(c != null)
	override protected def apply(data: MetricDataWithBugs[Double]): Double =
		100 * c.apply(
			data
				.metricValues
				.map(e => (e.bugValue, e.metricValue))
				.unzip
		)
}