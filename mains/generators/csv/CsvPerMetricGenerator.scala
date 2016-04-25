package mains.generators.csv

import common.rich.RichAll._
import common.rich.path.RichFile
import corpus.bugs.CorporaDataWithBugs
import corpus.rich.RichData._
import mains.generators.FileGenerator
import mains.generators.metricValues.FeatureParser
import mains.programs.{Configuration, Debug, MetricValuesParser}
import metrics.Metric

/** little helper class for generating data per metric */
private[csv] trait CsvPerMetricGenerator extends Debug {
	var builts: Set[FileGenerator] = Set()
	def build(m: Metric): CsvGenerator with ((CorporaDataWithBugs[Double], String) => Seq[Any])
	def createCsvGenerator(m: Metric) = {
		val built = build(m)
		builts += built
		new CsvGenerator(built.headers) with ((CorporaDataWithBugs[Double], String) => Unit) {
			override def unclearedFile: RichFile =
				CsvGenerator.defaultDirectory.addSubDir(built.simpleName).addFile(built.unclearedFile.name)
			override def apply(v1: CorporaDataWithBugs[Double], v2: String) { appendLine(built.apply(v1, v2)) }
		}
	}
	override def timedMain {
		val metrics = MetricValuesParser.getData.metrics
		val generators = metrics map createCsvGenerator
		for (f <- Configuration.features) {
			val data = FeatureParser load f
			val name = f.toString
			generators.foreach(_.apply(data, name))
		}
	}
}
