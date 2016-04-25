package mains.programs

import common.rich.RichAll._
import corpus.rich.RichData._
import mains.generators.metricValues.FeatureParser
import metrics._
import stats._

object CheckNormalityOfMetrics extends Debug {
	val correlation: Correlation = KendallTau
	lazy val rawData = FeatureParser load Configuration.defaultFeature
	def correlationToBugs(m: Metric): Seq[Double] =
		for (c <- rawData.corpora) yield KendallTau(c(m).metricValuesOnly, c.bugs)
	override def timedMain {
		for (m <- rawData.metrics)
			println("m has pvalue of: " + ShapiroWilk(correlationToBugs(m)))
	}
}
