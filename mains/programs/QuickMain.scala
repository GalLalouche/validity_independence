package mains.programs

import common.rich.CacheMap
import common.rich.RichAll._
import corpus.{CorporaData, CorpusData, MetricData}
import corpus.bugs.{CorporaDataWithBugs, _}
import corpus.rich.RichData._
import mains.generators.transformators.{SizeRankNormalizationTransformation, SizeLinearNormalizationTransformation, LatexNamedTransformator, _}
import mains.programs._
import metrics._
import org.apache.commons.math3.distribution.ChiSquaredDistribution
import plots.{RegressionBuilderDecorator, ScatterPlotBuilder}
import stats._
import stats.regression.LinearRegression
import corpus.bugs.Feature.MedianCommitsUntilNextChange
import mains.generators.metricValues.FeatureParser
import scala.util.Random

object QuickMain extends Debug {
	val pcaCache: CorpusData => Seq[Vector[Double]] = CacheMap[CorpusData, Seq[Vector[Double]]](PCA.getVectors)
	private val transformators = List(
		IdentityTransformator,
		SizeLinearNormalizationTransformation,
		SizeRankNormalizationTransformation
	)
	private def apply(rawData: CorporaDataWithBugs[Double], t: LatexNamedTransformator) = {
		val data = t applyWithBugs rawData
		(for (m <- data.metrics) yield {
			data.corpora // correlation to sizeMetric
				.map(c => {KendallTau(c.apply(m).metricValuesOnly, rawData.corpora.find(_.c == c.c).get.apply(NumberOfTokens).metricValuesOnly) })
				.mean
		}, for (m <- data.metrics) yield {
			data.corpora // correlation to feature
				.map(c => KendallTau(c.apply(m).metricValuesOnly, c.bugs))
				.mean
		})
	}
	sealed trait Nat

	case object Z extends Nat

	case class S(n: Nat) extends Nat

	val m = 20
	val n = 10
	val total = 100
	val attempts = 10000000
	override def timedMain: Unit = {
		MetricValuesParser.getData.filterLatest.log()
	}
}
