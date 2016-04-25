package corpus.bugs

import corpus.MetricData
import metrics.Metric

class MetricDataWithBugs[T](m: Metric, override val metricValues: Seq[MetricValueWithBugs[T]]) extends MetricData(m, metricValues) {
  // Maps the bug values
  def mapBugs[B](f: T => B): MetricDataWithBugs[B] = new MetricDataWithBugs(m, metricValues map (_ map f))
  // Keeps only entries of bugs matching this description
  def filterBugs(f: T => Boolean) = new MetricDataWithBugs(m, metricValues filter (e => f(e.bugValue)))
}

object MetricDataWithBugs {
  def apply[T](e: (Metric, Seq[MetricValueWithBugs[T]])) = new MetricDataWithBugs(e._1, e._2)
}