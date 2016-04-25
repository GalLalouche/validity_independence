package mains.generators.tables.bugs

import common.rich.RichAll._
import corpus.rich.RichData._
import mains.generators.metricValues.FeatureParser
import mains.generators.tables._
import mains.generators.transformators.{IdentityTransformator, SizeLinearNormalizationTransformation, SizeRankNormalizationTransformation}
import mains.programs.{Configuration, Debug}
import metrics.Metric
import stats.KendallTau

import scala.annotation.tailrec

object LatexBugCorrelationGeneratorAggregator extends LatexTableWithDynamicHeaders with Debug {
	private val statistics: Seq[Seq[Double] => Double] =
		Seq(_.mean, _.standardDeviation, _.median, _.medianAbsoluteDeviation, _.min, _.max)
	private class MetricFeatureRanges(name: String) extends FullTableGenerator {
		override def fileName: String = super.fileName + name
		val alignments = "lcccccc"
		val headers = Seq("Metric", "Means", "STDs", "Medians", "MADs", "Mins", "Maxs")
		override val caption = "The range differences between rank quintiles. " +
			"A value of zero, means that the metric stayed in the same quintile in all features; " +
			"A value of one means that it appeared in two adjacent quintiles; etc."
		def appendAll(xs: Seq[(Metric, Seq[Any])]) {
			writeLinesToLatex(xs.map(e => e._1 :: e._2))
		}
	}

	private def summarizeAcrossFeatures(xs: Seq[Double]): String = xs
		.range
		.mapTo(r => Math.abs(r._1 - r._2))
		.toInt
		.toString

	private def groupedBySizes[T](xs: Seq[T], groupSizes: Seq[Int]): Seq[Seq[T]] = {
		@tailrec
		def aux(xs: Seq[T], groupSizes: Seq[Int], res: List[Seq[T]]): List[Seq[T]] = (xs, groupSizes) match {
			case (Nil, Nil) => res
			case (Nil, _) | (_, Nil) =>
				throw new IllegalArgumentException(s"groupSizes total sum ${groupSizes.sum} is less than xs size ${xs.size}")
			case (_, currentSize :: gs) => aux(xs.drop(currentSize), gs, xs.take(currentSize) :: res)
		}
		aux(xs, groupSizes, List()).reverse
	}

	//TODO generalize this?
	private def rankNormalizeByQuintiles(xs: Seq[Double]): Seq[Double] = xs
		.normalizedByRankings
		.zipWithIndex // original index
		.sortBy(_._1)
		.mapTo(groupedBySizes(_, List(5, 5, 6, 5, 5))) // yeah yeah, it's 26 hard coded :\
		.zipWithIndex // sorted index
		.flatMap(e => e._1.map(_ -> e._2.toDouble)) // ((original_value, original_index), newValue)
		.sortBy(_._1._2) // restore original ordering
		.unzip._2 // return the new value

	override def timedMain {
		for ((t, s) <-
				 List(IdentityTransformator, SizeLinearNormalizationTransformation, SizeRankNormalizationTransformation)
					 .zip(List("Identity", "Linear", "Rank")))
			Configuration.goodFeatures.flatMap { f => // a sequence for each feature, of the stats per metric
				val dataWithBugs = t applyWithBugs FeatureParser.load(f)
				def statisticsForMetric(m: Metric): Seq[Double] = dataWithBugs
					.corpora
					.map(c => KendallTau(c.bugs, c(m).metricValuesOnly))
					.mapTo(e => statistics.map(_.apply(e))) // applies statistics
				dataWithBugs.metrics zip dataWithBugs.metrics
					.par.map(statisticsForMetric).seq
					.transpose.map(rankNormalizeByQuintiles).transpose
			}.groupBy(_._1) // group by metric
				.map(
					e => e._1 -> e._2
						.map(_._2).transpose
						.map(summarizeAcrossFeatures)) // takes the range (or ellipse, or anything)
				.toVector
				.sortBy(_._1.shortName.captureWith(".*?([A-Z]+).*?".r)) // resorts the metrics by name; regex assure correct location of NOT due to $s
				.applyAndReturn(new MetricFeatureRanges(s).appendAll)
	}
}
