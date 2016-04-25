package corpus.rich

import common.rich.RichAll._
import corpus._
import corpus.bugs._
import corpus.rich.RichCorpusData.CorpusDataReprBuilder
import corpus.rich.RichMetricData._
import il.ac.technion.cs.ssdl.sablecc.GitLogsRecord
import metrics.Metric
import parsers.git.GitRecordsParser

abstract class RichCorpusData[Repr <: CorpusData, Elem <: MetricData, TransposedType <: MetricValuesForFile,
SubElem <: MetricValue] private(protected val repr: Repr) {
	self: CorpusDataReprBuilder[Repr, Elem, TransposedType, SubElem] =>

	def withData(data: CorpusData) = {
		require(repr.c == data.c)
		buildFromSuperElements(data.metricValues)
	}

	def apply(m: Metric) = {
		val $ = repr.metricValues.find(_.m == m)
		if ($.isDefined)
			$.get
		else
			throw new IllegalArgumentException(s"Cannot find metric: $m, metrics are $metrics}")
	}

	private def aggregateMetricValuesPerFile(f: Elem => MetricData): Repr =
		buildRepr(repr,
			elements
				.zip(elements map f)
				.map(e => buildFromSuperElement(e._1, e._2)))

	def sortMetrics(sorter: Seq[Elem] => Seq[Elem]): Repr = buildFromData(sorter(elements))
	def getAverageValuesPerFile = aggregateMetricValuesPerFile(_.getAverageValuesPerFile)
	def getMedianValuesPerFile = aggregateMetricValuesPerFile(_.getMedianValuesPerFile)
	def getSumValuesPerFile = aggregateMetricValuesPerFile(_.getSumValuesPerFile)

	def metricValuesOnly = elements map (_.metricValuesOnly)

	def metrics = repr.metricValues map (_.m)

	def metricNames = metrics map (_.shortName)

	def corpusName = repr.c.index.toString

	def filterLatest: Repr = filterRevisionsBySet(createFilteringSet(_.last))

	def filterFirst: Repr = filterRevisionsBySet(createFilteringSet(_.head))

	def removeTests: Repr = filterByFileCommit(_.file.toLowerCase.matches(".*?tests?.java$") == false)
	def onlyTests: Repr = filterByFileCommit(_.file.toLowerCase.matches(".*?tests?.java$"))

	def ignoreMetrics(metricsToIgnore: Set[Metric]) =
		buildRepr(repr, elements.filterNot(e => metricsToIgnore.contains(e.m)))

	def useMetrics(metricsToUse: Set[Metric]): Repr = {
		val metricsSet = metrics.toSet
		require(metricsToUse <= metricsSet)
		ignoreMetrics(metricsSet \ metricsToUse)
	}

	private def createFilteringSet(f: Seq[GitLogsRecord] => GitLogsRecord): Set[FileCommit] =
		GitRecordsParser
			.getLogsByFileByDate(repr.c.logFile)
			.map(f apply _._2)
			.map(FileCommit.apply)
			.toSet

	private def filterRevisionsBySet(set: Set[FileCommit]): Repr = filterByFileCommit(set.contains)

	private def filterByFileCommit(p: FileCommit => Boolean): Repr =
		transpose
			.filter(p)
			.mapTo(untranspose)
			.mapTo(buildRepr(repr, _))

	def transpose: Seq[TransposedType] = {
		assert(repr.metricValues.map(_.metricValues).hasSameValues(_ map (e => (e.file, e.revision))))
		transpose(extractSubElements)
	}

	private def transpose(xs: Seq[Seq[SubElem]]): Seq[TransposedType] =
		xs.transpose
			.map(e => buildTransposedType(e.head, e.map(_.metricValue) zip metrics))

	private def untranspose(xs: Seq[TransposedType]) = {
		assert(xs.forall(_.metrics == metrics))
		val samples = xs map takeSample
		val rawData = xs.map(_.metricValues).transpose
		metrics zip rawData map {
			case (metric, dataForMetric) => buildElement(metric, samples zip dataForMetric.map(_.metricValue))
		}

	}

	def ++(other: Repr) = {
		require(repr.c != other.c)
		buildRepr(new CorpusData(new UnifiedCorpus(repr.c, other.c), List(elements.head)),
			elements zip extractElements(other) map (e => concatenateElements(e._1, e._2)))
	}

	def join(other: CorpusData): Repr = {
		val richOther = RichCorpusData.richCorpusData(other)
		require(other.c == repr.c)
		require(richOther.metrics.toSet isDisjointTo metrics.toSet,
			s"Cannot join metric set ${richOther.metrics.toSet} with metric set ${metrics.toSet} as they are not disjoint " +
				s"(${richOther.metrics.toSet.intersect(metrics.toSet)} are common)")
		val tts = transpose zip richOther.transpose map {
			case (ms, os) =>
				require(ms.file == os.file)
				require(ms.revision == os.revision)
				buildTransposedType(takeSample(ms), ms.zippedValues ++ os.zippedValues)
		}
		buildFromTransposedType(tts)
	}

	def normalizedByRankings: Repr = buildFromSuperElements(elements.map(_.normalizedByRankings))

	def sample(n: Int): (Repr, Repr) = ???
	def sample(percentage: Double): (Repr, Repr) = sample((percentage * repr.metricValues.head.metricValues.size).toInt)
	def toRFormat = transpose.map(getStringSequence).map(_.mkString("", ",", ""))

	def getVectors = transpose.map(_.rawValuesOnly.toVector)
}

object RichCorpusData {

	trait CorpusDataReprBuilder[Repr <: CorpusData, Elem <: MetricData, TransposedType <: MetricValuesForFile, SubElem <: MetricValue]
		extends SimpleDataReprBuilder[CorpusData, MetricData, Repr, Elem] {
		protected def extractSubElements: Seq[Seq[SubElem]]
		protected def takeSample(tt: TransposedType): SubElem
		protected def getStringSequence(tt: TransposedType): Seq[String]
		protected def buildTransposedType(sample: SubElem, data: Seq[(Double, Metric)]): TransposedType
		protected def buildElement(m: Metric, data: Seq[(SubElem, Double)]): Elem
		protected def concatenateElements(e1: Elem, e2: Elem): Elem
		protected def buildFromTransposedType(tts: Seq[TransposedType]): Repr
	}

	trait CorpusDataBuilder extends CorpusDataReprBuilder[CorpusData, MetricData, MetricValuesForFile, MetricValue] {
		override protected def extractElements(cd: CorpusData) = cd.metricValues
		override protected def buildFromSuperElement(e: MetricData, other: MetricData): MetricData = e.withData(other)
		override protected def buildRepr(c: CorpusData, xs: Seq[MetricData]) = new CorpusData(c.c, xs)
		protected def getStringSequence(tt: MetricValuesForFile): Seq[String] = tt.rawValuesOnly.map(_.toString)
		override protected def buildTransposedType(sample: MetricValue, data: Seq[(Double, Metric)]) =
			new MetricValuesForFile(sample, data)
		override protected def extractSubElements: Seq[Seq[MetricValue]] = repr.metricValues.map(_.metricValues)
		override protected def buildElement(m: Metric, data: Seq[(MetricValue, Double)]): MetricData =
			new MetricData(m, data map (e => e._1.withMetricValue(e._2)))
		override protected def takeSample(tt: MetricValuesForFile): MetricValue = tt.metricValues.head
		override protected def concatenateElements(e1: MetricData, e2: MetricData): MetricData = e1 ++ e2
		protected def buildFromTransposedType(tts: Seq[MetricValuesForFile]) = RichCorpusData.buildFromTransposedData(repr.c, tts)
	}

	implicit def richCorpusData($: CorpusData) =
		new RichCorpusData[CorpusData, MetricData, MetricValuesForFile, MetricValue]($) with CorpusDataBuilder

	trait CorpusDataWithBugsBuilder[T]
		extends CorpusDataReprBuilder[CorpusDataWithBugs[T], MetricDataWithBugs[T],
			MetricValuesForFileWithBugs[T], MetricValueWithBugs[T]] {
		override protected def extractElements(cd: CorpusDataWithBugs[T]) = cd.metricValues
		override protected def buildFromSuperElement(e: MetricDataWithBugs[T], other: MetricData) = e.withData(other)
		override protected def buildRepr(c: CorpusData, xs: Seq[MetricDataWithBugs[T]]) = new CorpusDataWithBugs[T](c.c, xs)
		override protected def buildTransposedType(sample: MetricValueWithBugs[T], data: Seq[(Double, Metric)]) =
			new MetricValuesForFileWithBugs[T](sample, sample.bugValue, data)
		override protected def extractSubElements = repr.metricValues.map(_.metricValues)
		override protected def buildElement(m: Metric, data: Seq[(MetricValueWithBugs[T], Double)]) =
			new MetricDataWithBugs[T](m, data map (e => e._1.withMetricValue(e._2)))
		override protected def takeSample(tt: MetricValuesForFileWithBugs[T]) = tt.metricValues.head
		override protected def concatenateElements(e1: MetricDataWithBugs[T], e2: MetricDataWithBugs[T]) = e1 ++ e2
		override protected def buildFromTransposedType(tts: Seq[MetricValuesForFileWithBugs[T]]) =
			RichCorpusData.buildFromTransposedData(repr.c, tts)
		protected def getStringSequence(tt: MetricValuesForFileWithBugs[T]): Seq[String] = (tt.bugValue :: tt.rawValuesOnly.toList).map(_.toString)
	}

	implicit def richCorpusDataWithBugs[T]($: CorpusDataWithBugs[T]) =
		new RichCorpusData[CorpusDataWithBugs[T], MetricDataWithBugs[T], MetricValuesForFileWithBugs[T], MetricValueWithBugs[T]]($) with CorpusDataWithBugsBuilder[T]

	//TODO fix code duplication between these two
	def buildFromTransposedData(c: Corpus, data: Seq[MetricValuesForFile]): CorpusData = {
		def transpose(mvs: Seq[MetricValuesForFile]): Seq[MetricData] = {
			val metrics = mvs(0).metrics
			assert(mvs.forall(_.metrics == metrics))
			mvs
				.map(_.metricValues)
				.transpose
				.zip(metrics)
				.map(e => new MetricData(e._2, e._1))
		}
		new CorpusData(c, transpose(data))
	}

	def buildFromTransposedData[T](c: Corpus, data: Seq[MetricValuesForFileWithBugs[T]]): CorpusDataWithBugs[T] = {
		def transpose(mvs: Seq[MetricValuesForFileWithBugs[T]]): Seq[MetricDataWithBugs[T]] = {
			require(mvs.nonEmpty)
			val metrics = mvs(0).metrics
			assert(mvs.forall(_.metrics == metrics))
			mvs
				.map(_.metricValues)
				.transpose
				.zip(metrics)
				.map(e => new MetricDataWithBugs(e._2, e._1))
		}
		new CorpusDataWithBugs(c, transpose(data))
	}
}
