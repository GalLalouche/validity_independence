package metrics.post

import metrics.Metric

/**
* these are metrics that can only be run after certain information on the entire corpus has been gathered
*/
trait PostMetric[T] {
  def createMetric(t: T): Metric
}
