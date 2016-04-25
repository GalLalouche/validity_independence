package corpus.bugs

import corpus.{FileCommit, MetricValuesForFile}
import metrics.Metric

class MetricValuesForFileWithBugs[T](fc: FileCommit, val bugValue: T, _metricValues: Seq[(Double, Metric)])
	extends MetricValuesForFile(fc, _metricValues) {
	def metricValuesWithBugs: Seq[MetricValueWithBugs[T]] = metricValues map (MetricValueWithBugs(_, bugValue))
	def withBugValue[U](newValue: U) = new MetricValuesForFileWithBugs(this, newValue, _metricValues)
	override def metricValues: Seq[MetricValueWithBugs[T]] =
		zippedValues map (e => new MetricValueWithBugs[T](this, e._1, bugValue))
}

object MetricValuesForFileWithBugs {
	def apply[T](bv: MetricValuesForFileWithBugs[T], mv: MetricValuesForFile) =
		new MetricValuesForFileWithBugs[T](mv, bv.bugValue, mv.zippedValues)
	def apply[T](mv: MetricValuesForFile, bv: SimpleFileCommitValue[T]) = {
		require(mv.file == bv.file, s"mv file: ${mv.file}, bv file: ${bv.file}")
		require(mv.revision == bv.revision, s"mv revision: ${mv.revision}, bv revision: ${bv.revision}")
		new MetricValuesForFileWithBugs(mv, bv.bugValue, mv.rawValuesOnly zip mv.metrics)
	}
}