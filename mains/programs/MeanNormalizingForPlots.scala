package mains.programs

import common.rich.RichAll._
import corpus._
import corpus.rich.RichData._
import java.io._

import mains.generators.transformators.{ScaleTransformator, ShiftTransformator, LogTransformator}

object MeanNormalizingForPlots extends Debug {
	def normalizeCorporaWide(data: CorporaData): CorporaData = {
		val metrics = Configuration.softwareMetrics
		(for (m <- metrics) yield {
			println("Normalizing metric " + m)
			val metricWithM: CorporaData = data.useMetrics(Set(m))
			val loggedData = metricWithM.metricValuesOnly.flatten.flatten.rerange2Positives.map(Math.log)
			val mean = loggedData.median.log(e => s"Median: $e")
			val std = loggedData.standardDeviation.log(e => s"STD: $e")
			metricWithM.map(_.map(_
				.flatMap(_.rerange2Positives)
				.map(Math.log)
				.map(e => (e - mean) / std)
				.map(Math.exp).ensuring(_.metricValuesOnly.forall(_.isRealDouble))))
		}).reduce { (current, next) =>
			new CorporaData(current.corpora.zip(next.corpora)
				.map(e => new CorpusData(e._1.c, e._1.metricValues ++ e._2.metricValues)))
		}
	}
	override def timedMain {
		val data = MetricValuesParser.getData.getAverageValuesPerFile
		val normalized = normalizeCorporaWide(data)
		MetricValuesParser.writeMetricValuesToDisk(normalized, Configuration.rawValuesFolder.addSubDir("normalized"))
	}
}
