package mains.generators.csv

import corpus.bugs.CorporaDataWithBugs
import metrics.Metric
import stats.KendallTau
import metrics.NumberOfTokens
import corpus.rich.RichData._

class SizeToFeatureCorrelation(m: Metric) {
	def apply(data: CorporaDataWithBugs[Double]) = {
		for (c <- data.corpora) yield 
			KendallTau(c(m).metricValuesOnly, c(NumberOfTokens).metricValuesOnly) ->
			KendallTau(c(m).metricValuesOnly, c.bugs)
	}
}

object SizeToFeatureCorrelation {
	def apply(data: CorporaDataWithBugs[Double], m: Metric) = 
		new SizeToFeatureCorrelation(m)(data)
}