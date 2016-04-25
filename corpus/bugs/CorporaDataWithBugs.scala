package corpus.bugs

import corpus.rich.RichCorpusData
import corpus.{ComposedSequence, CorporaData}
import mains.generators.transformators.CorpusTransformator
import corpus.rich.RichCorpusData._
import common.rich.RichAll._

class CorporaDataWithBugs[T](override val corpora: Seq[CorpusDataWithBugs[T]]) extends CorporaData(corpora) {
	def mapBugs[U](f: T => U): CorporaDataWithBugs[U] = new CorporaDataWithBugs(corpora map (_ mapBugs f))
	def filterBugs(f: T => Boolean): CorporaDataWithBugs[T] = new CorporaDataWithBugs(corpora map (_ filterBugs f))
	def applyBugs(data: CorporaData): CorporaDataWithBugs[T] =
		new CorporaDataWithBugs[T](corpora zip data.corpora map (e => e._1 applyBugs e._2))
	def sample(d: Double) = {
		corpora.map { data =>
			val size = data.metricValues.head.metricValues.size
			data
				.transpose
				.shuffle
				.splitAt((size * d).toInt)
				.map(RichCorpusData.buildFromTransposedData(data.c, _))
		}.unzip.map(new CorporaDataWithBugs[T](_))
	}
}
