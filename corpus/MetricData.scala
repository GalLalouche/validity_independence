package corpus

import metrics.Metric

/**
 * All values for a metric on a given corpus
 */
class MetricData(val m: Metric, val metricValues: Seq[MetricValue]) {
  require(m != null)
  require(metricValues nonEmpty)
  override def toString = "Data for " + m.shortName
}