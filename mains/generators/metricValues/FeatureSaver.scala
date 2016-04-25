package mains.generators.metricValues

import corpus.CorporaData
import corpus.bugs.{CorporaDataWithBugs, CorpusDataWithBugs, Feature}
import mains.programs.MetricValuesParser

trait FeatureSaver {
	protected def save(f: Feature, data: CorpusDataWithBugs[Double]): Unit

	def save(f: Feature, data: CorporaData) { save(f, f joinBugs data) }

	def save(f: Feature, data: CorporaDataWithBugs[Double]) { data.corpora.foreach(save(f, _)) }

	def save(f: Feature) { this.save(f, f joinBugs MetricValuesParser.getData) }
}
