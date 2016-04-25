package corpus

import scala.collection.generic.CanBuildFrom

/**
 * A single sampling of a metric for a given file commit
 */
class MetricValue(fc: FileCommit, val metricValue: Double) extends FileCommit(fc) {
	def mapMetricValue(f: Double => Double) = withMetricValue(f(metricValue))
	def withMetricValue(newValue: Double) = new MetricValue(this, newValue)
	override def toString: String = s"$metricValue, $file, ${revision.take(6)}"
}
