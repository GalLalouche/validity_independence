package corpus.rich

import common.rich.RichT._
import common.rich.collections.RichTraversableDouble._
import corpus._
import corpus.bugs.{MetricDataWithBugs, MetricValueWithBugs}

abstract class RichMetricData[Repr <: MetricData, Elem <: MetricValue] private(protected val repr: Repr) {
	self: RichMetricData.MetricDataReprBuilder[Repr, Elem] =>

	override def data = extractElements(repr).map(_.metricValue)
	override protected def buildFromSubElement(e: Elem, d: Double): Elem = buildFromSuperElement(e, e.withMetricValue(d))
	def metricValuesOnly = data

	def withData(data: MetricData): Repr = {
		def extractFileCommitValues(data: MetricData) = elements.map(e => e.file -> e.revision)
		require(extractFileCommitValues(repr) == extractFileCommitValues(data))
		buildFromSuperElements(data.metricValues, data)
	}

	private def aggregateMetricValuesPerFile(f: Seq[Double] => Double): Repr = elements
		.groupBy(_.file)
		.map(e => e._2.head -> f(e._2.map(_.metricValue)))
		.map(e => (e._1, e._1.withoutRevision, e._2))
		.map(e => buildFromSuperElement(e._1, new MetricValue(e._2, e._3)))
		.toVector
		.mapTo(buildRepr(repr, _))

	def getAverageValuesPerFile: Repr = aggregateMetricValuesPerFile(_.mean)
	def getMedianValuesPerFile: Repr = aggregateMetricValuesPerFile(_.median)
	def getSumValuesPerFile: Repr = aggregateMetricValuesPerFile(_.sum)

	def ++(other: Repr) = {
		require(repr.m == other.m)
		buildRepr(repr, elements ++ extractElements(other))
	}

	def normalizedByRankings: Repr = flatMap(_.normalizedByRankings)
}


object RichMetricData {

	trait MetricDataReprBuilder[Repr <: MetricData, Elem <: MetricValue]
		extends DataReprBuilder[MetricData, MetricValue, Repr, Elem, Double]

	trait MetricDataBuilder extends MetricDataReprBuilder[MetricData, MetricValue] {
		override protected def buildFromSuperElement(e: MetricValue, mv: MetricValue): MetricValue =
			mv
		override protected def extractElements(md: MetricData) = md.metricValues
		override protected def buildRepr(md: MetricData, xs: Seq[MetricValue]) = new MetricData(md.m, xs)
	}

	implicit def richMetricData($: MetricData) = new RichMetricData[MetricData, MetricValue]($) with MetricDataBuilder

	trait MetricDataWithBugsBuilder[T] extends MetricDataReprBuilder[MetricDataWithBugs[T], MetricValueWithBugs[T]] {
		override protected def buildFromSuperElement(e: MetricValueWithBugs[T], mv: MetricValue): MetricValueWithBugs[T] =
			new MetricValueWithBugs[T](mv, mv.metricValue, e.bugValue)
		override protected def buildFromSubElement(e: MetricValueWithBugs[T], newValue: Double) = e.withMetricValue(newValue)
		override protected def extractElements(md: MetricDataWithBugs[T]) = md.metricValues
		override protected def buildRepr(md: MetricData, xs: Seq[MetricValueWithBugs[T]]): MetricDataWithBugs[T] =
			new MetricDataWithBugs[T](md.m, xs)
	}

	implicit def richMetricDataWithBugs[T]($: MetricDataWithBugs[T]) =
		new RichMetricData[MetricDataWithBugs[T], MetricValueWithBugs[T]]($) with MetricDataWithBugsBuilder[T]
}
