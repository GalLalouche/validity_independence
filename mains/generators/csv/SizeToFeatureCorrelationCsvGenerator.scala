package mains.generators.csv

import corpus.bugs.CorporaDataWithBugs
import corpus.rich.RichData.richCorporaData
import corpus.rich.RichData.richCorpusData
import corpus.rich.RichData.richMetricData
import mains.generators.metricValues.FeatureParser
import mains.programs.Configuration
import mains.programs.Debug
import mains.programs.MetricValuesParser
import metrics.Metric
import metrics.NumberOfTokens
import stats.KendallTau

class SizeToFeatureCorrelationCsvGenerator(m: Metric, sizeCorrelation: Seq[Double])
	extends CsvGenerator(Configuration.corpora.map(_.index.toString)) {
	def apply(data: CorporaDataWithBugs[Double], featureName: String) = {
		for (c <- data.corpora) yield KendallTau(c(m).metricValuesOnly, c(NumberOfTokens).metricValuesOnly) ->
			KendallTau(c(m).metricValuesOnly, c.bugs)
	}

	override lazy val file = {
		unclearedFile.clear.appendLine(headers mkString ",")
		super.appendLine(sizeCorrelation)
		super.unclearedFile
	}
}

object SizeToFeatureCorrelationCsvGenerator extends Debug {
	override def timedMain {
		val data = MetricValuesParser.getData
		val csvGenerators = data.metrics.map(m =>
			m -> data.corpora.map(c =>
				KendallTau(c(m).metricValuesOnly, c(NumberOfTokens).metricValuesOnly))).map(e => new SizeToFeatureCorrelationCsvGenerator(e._1, e._2))
		for (
			(data, featureName) <- Configuration
				.features
				.map(f => FeatureParser.load(f) -> f.toString)
		) Configuration.features.map(FeatureParser.load).foreach { data =>
			csvGenerators.foreach(_.apply(data, featureName))
		}
	}
}
