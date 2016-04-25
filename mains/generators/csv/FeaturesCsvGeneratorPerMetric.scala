package mains.generators.csv

import common.rich.primitives.RichDouble._
import corpus.bugs.CorporaDataWithBugs
import corpus.rich.RichData._
import mains.programs.Configuration
import metrics.Metric
import stats.KendallTau

// takes the mean metric value per corpus
class FeaturesCsvGeneratorPerMetric(m: Metric)
	extends CsvGenerator("Feature" :: Configuration.corpora.map(_.index.toString).toList)
	with ((CorporaDataWithBugs[Double], String) => Seq[Any]) {
	override def fileName = "feature_values_for_" + m.shortName
	def apply(data: CorporaDataWithBugs[Double], featureName: String) = {
		val correlationsToBugs = for (c <- data.corpora) yield {
			val metricValues = c(m).metricValuesOnly
			val bugs = c.bugs
			KendallTau(metricValues, bugs)
		}
		featureName.toString :: correlationsToBugs.toList.map(_ * 100).map(_.withPrecision(2))
	}
}

object FeaturesCsvGenerator extends CsvPerMetricGenerator {
	override def build(m: Metric) = new FeaturesCsvGeneratorPerMetric(m)
}
