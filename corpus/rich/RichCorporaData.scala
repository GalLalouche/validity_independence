package corpus.rich

import common.rich.RichAll._
import corpus._
import corpus.bugs.{CorporaDataWithBugs, CorpusDataWithBugs}
import corpus.rich.RichCorporaData.CorporaDataReprBuilder
import corpus.rich.RichCorpusData._
import corpus.rich.RichMetricData._
import metrics.Metric
import stats.KendallTau

abstract class RichCorporaData[Repr <: CorporaData, Elem <: CorpusData] private(protected val repr: Repr) {
	self: CorporaDataReprBuilder[Repr, Elem] =>
	def metricValuesOnly = elements map (_.metricValuesOnly)

	def filterLatest: Repr = buildFromSuperElements(elements map (_.filterLatest))
	def filterFirst: Repr = buildFromSuperElements(elements map (_.filterFirst))

	def getAverageValuesPerFile: Repr = buildFromSuperElements(elements map (_.getAverageValuesPerFile))
	def getMedianValuesPerFile: Repr = buildFromSuperElements(elements map (_.getMedianValuesPerFile))

	def corporaNames = elements map (_ corpusName)
	def metricNames = elements.head.metricNames
	def metrics = elements.head.metrics

	def ignoreMetrics(metricsToIgnore: Metric*): Repr = ignoreMetrics(metricsToIgnore.toSet)
	def ignoreMetrics(metricsToIgnore: Set[Metric]) =
		buildFromSuperElements(elements map (_ mapTo (e => new CorpusData(e.c, e.metricValues filterNot (metricsToIgnore contains _.m)))))

	def useMetrics(metrics: Metric*): Repr = useMetrics(metrics.toSet)

	def useMetrics(metricsToUse: Set[Metric]): Repr = {
		ignoreMetrics(metrics.toSet \ metricsToUse)
	}
	def join(other: Repr): Repr = {
		buildRepr(repr, elements zip other.corpora map {
			case (xs, ys) => buildFromSuperElement(xs, xs join ys)
		})
	}

	def withoutTests: Repr = buildFromSuperElements(elements map (_.removeTests))
	def onlyTests: Repr = buildFromSuperElements(elements map (_.onlyTests))

	def concatenated: Elem = elements reduce concatenateElements

	def normalizedByRankings: Repr = buildFromSuperElements(elements.map(_.normalizedByRankings))

	def correlationBetween(m1: Metric, m2: Metric): Double =
		repr.corpora.map(c => KendallTau(c(m1).metricValuesOnly, c(m2).metricValuesOnly)).mean

	def average(f: Elem => Number): Double = elements.map(f(_).doubleValue).sum / elements.size

	def find(e: Corpus) = elements find (_.c == e)

	def getVectors = elements map (_.getVectors)
}

object RichCorporaData {

	trait CorporaDataReprBuilder[Repr <: CorporaData, Elem <: CorpusData]
		extends SimpleDataReprBuilder[CorporaData, CorpusData, Repr, Elem] {
		def concatenateElements(e1: Elem, e2: Elem): Elem

	}

	trait CorporaDataBuilder extends CorporaDataReprBuilder[CorporaData, CorpusData] {
		override protected def extractElements(repr: CorporaData): Seq[CorpusData] = repr.corpora
		override protected def buildFromSuperElement(e: CorpusData, se: CorpusData): CorpusData = se
		override protected def buildRepr(mm: CorporaData, xs: Seq[CorpusData]): CorporaData = new CorporaData(xs)
		override def concatenateElements(e1: CorpusData, e2: CorpusData): CorpusData = e1 ++ e2
	}

	implicit def richCorporaData($: CorporaData) =
		new RichCorporaData[CorporaData, CorpusData]($) with CorporaDataBuilder

	trait CorporaDataWithBugsBuilder[T] extends CorporaDataReprBuilder[CorporaDataWithBugs[T], CorpusDataWithBugs[T]] {
		override protected def extractElements(repr: CorporaDataWithBugs[T]) = repr.corpora
		override protected def buildFromSuperElement(e: CorpusDataWithBugs[T], se: CorpusData) =
			e.applyBugs(se)
		override protected def buildRepr(mm: CorporaData, xs: Seq[CorpusDataWithBugs[T]]) = new CorporaDataWithBugs[T](xs)
		override def concatenateElements(e1: CorpusDataWithBugs[T], e2: CorpusDataWithBugs[T]) = e1 ++ e2
	}

	implicit def richCorporaDataWithBugs[T]($: CorporaDataWithBugs[T]) =
		new RichCorporaData[CorporaDataWithBugs[T], CorpusDataWithBugs[T]]($) with CorporaDataWithBugsBuilder[T] {
			override def getAverageValuesPerFile: CorporaDataWithBugs[T] = {
				System.err.println("getAverageValuesPerFile called on data with feature values; are you sure you want to use this method instead of an explicit feature?")
				super.getAverageValuesPerFile
			}
		}
}

