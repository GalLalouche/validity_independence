package corpus.bugs

import corpus.{FileCommit, FileCommitValue, MetricValue}

class MetricValueWithBugs[T](fc: FileCommit, metricValue: Double, val bugValue: T)
  extends MetricValue(fc, metricValue) with FileCommitValue[T, MetricValueWithBugs[T]] {
  protected val value = bugValue
  override def withMetricValue(newValue: Double) = new MetricValueWithBugs[T](this, newValue, bugValue)
  def withBugValue[B](newValue: B) = new MetricValueWithBugs[B](this, metricValue, newValue)
  override def toString: String = bugValue + ", " + super[MetricValue].toString
}

object MetricValueWithBugs {
  def apply[T](mv: MetricValue, bv: SimpleFileCommitValue[T]) = {
    require(mv.file == bv.file)
    require(mv.revision == bv.revision)
    new MetricValueWithBugs(mv, mv.metricValue, bv.bugValue)
  }
  def apply[T](mv: MetricValue, bv: T) = new MetricValueWithBugs[T](mv, mv.metricValue, bv)

  implicit def bf[T, B]: ((MetricValueWithBugs[T], B) => MetricValueWithBugs[B]) =
    (bv, newValue) => new MetricValueWithBugs[B](bv, bv.metricValue, newValue)
}
