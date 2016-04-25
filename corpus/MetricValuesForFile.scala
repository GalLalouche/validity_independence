package corpus

import metrics.Metric

class MetricValuesForFile(fc: FileCommit, val zippedValues: Seq[(Double, Metric)]) extends FileCommit(fc) with ComposedSequence[Double, MetricValuesForFile] {
	require(zippedValues.nonEmpty)
	def metricValues: Seq[MetricValue] = zippedValues map (e => new MetricValue(this, e._1))
	protected def data = zippedValues map (_._1)
	protected def buildFromData(data: Seq[Double]) =
		new MetricValuesForFile(this, data zip zippedValues map (_._2))
	def metrics: Seq[Metric] = zippedValues map (_._2)
	def metricNames = metrics map (_.shortName)
	def rawValuesOnly = zippedValues map (_._1)
	def apply(m: Metric): Double = zippedValues.find(_._2 == m).get._1
}
