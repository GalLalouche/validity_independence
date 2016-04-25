package mains.generators.metricValues

import common.rich.RichAll._
import corpus._
import corpus.rich.RichCorpusData
import corpus.rich.RichCorpusData._
import mains.programs._
import metrics.Metric
import metrics.post.{DepthOfInheritance, NumberOfChildren}
import parsers.git.GitRecordsParser

/**
 * This class parses all the java classes in a given corpus and assigns each file a metric value
 */
object MetricValuesGenerator extends Debug {
	private def apply(c: Corpus, metrics: Seq[Metric]): CorpusData = {
		val graphMetrics = List(NumberOfChildren, DepthOfInheritance)
		val graph = timed("building class graph") {RevisionsGraph(c)}
		timed(s"Calculating metrics for corpus ${c.fullName} (${c.index})") {
			GitRecordsParser.parse(c.logFile)
				.withPercentage(c.size)
				.filterNot(_.node.isInterface)
				.tryMap(br =>
				(FileCommit.apply(br),
					(metrics ++ graphMetrics.map(e => e.createMetric(graph(br.revision))))
						.par.map(_.apply(br.node)).seq),
					br => s"Failed in corpus ${c.fullName} in file ${br.file} in revision ${br.revision}")
				.toVector
				.map(e => new MetricValuesForFile(e._1, e._2 zip (metrics ++ graphMetrics.map(e => e.createMetric(graph.sample)))))
				.mapTo(RichCorpusData.buildFromTransposedData(c, _).flatMap(_.sortBy(_.m.shortName)))
		}
	}

	// generates all data for all corpora
	override def timedMain {
		timed("Generating data across ALL corpora")({
			val metrics = Configuration.softwareMetrics
			Configuration.corpora
				.iterator // allows saving the corpus data as soon as it's done
				.verify(_.isValid, (e, index) => s"Project $e is invalid")
				.map(MetricValuesGenerator.this(_, metrics))
				.foreach(MetricValuesParser writeMetricValuesToDisk)
		})
	}
}