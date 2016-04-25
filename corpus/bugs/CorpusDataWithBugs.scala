package corpus.bugs

import corpus._
import corpus.rich.RichCorpusData
import corpus.rich.RichData._

class CorpusDataWithBugs[T](c: Corpus, override val metricValues: Seq[MetricDataWithBugs[T]]) extends CorpusData(c, metricValues) {
	def mapBugs[U](f: T => U) = new CorpusDataWithBugs(c, metricValues map (_ mapBugs f))
	def flatMapBugs[U](f: Seq[T] => Seq[U]) = {
		val newBugs = f(this.bugs)
		RichCorpusData.buildFromTransposedData(c, this.transpose zip newBugs map (e => e._1.withBugValue(e._2)))
	}
	def filterBugs(f: T => Boolean) = new CorpusDataWithBugs(c, metricValues map (_ filterBugs f))

	def applyBugs(data: CorpusData): CorpusDataWithBugs[T] =
		RichCorpusData.buildFromTransposedData(c, this.transpose zip data.transpose map (e => MetricValuesForFileWithBugs.apply(e._1, e._2)))
	def bugs = metricValues.head.metricValues.map(_.bugValue)
}
