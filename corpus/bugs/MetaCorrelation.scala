package corpus.bugs

import common.rich.RichAll._
import corpus.rich.RichData._
import mains.generators.metricValues.FeatureCache
import mains.generators.transformators._
import mains.programs.Configuration
import metrics._
import metrics.post.NumberOfChildren
import stats._

case class MetaCorrelation(correlationsToSize: Seq[Double], correlationsToFeature: Seq[Double],
    feature: Feature, t: LatexNamedTransformator, sizeMetric: Metric) {
  require(correlationsToFeature.size == correlationsToSize.size)
  def zipped = correlationsToSize zip correlationsToFeature
  def pearson = Pearson(correlationsToSize, correlationsToFeature)
  def ++(o: MetaCorrelation) = {
    require(sizeMetric == o.sizeMetric)
    require(feature == o.feature)
    MetaCorrelation(correlationsToSize ++ o.correlationsToSize, correlationsToFeature ++ o.correlationsToFeature, feature, null, sizeMetric)
  }
}
object MetaCorrelation {
  private val correlation: Correlation = KendallTau
  def apply(f: Feature, t: LatexNamedTransformator, sizeMetric: Metric): MetaCorrelation = {
    val rawData = FeatureCache.load(f).withoutTests
    val data = t applyWithBugs rawData
    val sizeCorrelation = data.metrics.map(m => data.corpora
        .map(c => {correlation(c.apply(m).metricValuesOnly, rawData.corpora.find(_.c == c.c).get.apply(sizeMetric).metricValuesOnly) }).mean)
    val featureCorrelation = data.metrics.map(m => data.corpora
        .map(c => correlation(c.apply(m).metricValuesOnly, c.bugs)).mean)
    MetaCorrelation(correlationsToSize = sizeCorrelation, correlationsToFeature = featureCorrelation, f, t, sizeMetric)
  }

  def sizeMetrics(sizeMetric: Metric): List[Metric] = List(sizeMetric, GZipRegularity, McCabesWithShortCircuit, NumberOfChildren)
  private def apply2(f: Feature, t: LatexNamedTransformator, sizeMetric: Metric): Seq[MetaCorrelation] = {
    sizeMetrics(sizeMetric).map(apply(f, t, _))
  }
  val transformators: List[LatexNamedTransformator] = List(IdentityTransformator, SizeLinearNormalizationTransformation, SizeRankNormalizationTransformation)
  def findSizeMetric(ts: Seq[LatexNamedTransformator]): Metric =
    ts.mapDefined(_.safeCast[SizeRankNormalizationTransformation]).head.sizeMetric
  def apply(f: Feature, _transformators: Seq[LatexNamedTransformator] = transformators): Seq[MetaCorrelation] = {
    _transformators.flatMap(t => sizeMetrics(findSizeMetric(_transformators)).map(apply(f, t, _)))
  }
  val features: Seq[Feature] = Configuration.goodFeatures
  def getAll(_transformators: Seq[LatexNamedTransformator]): Seq[MetaCorrelation] =
    features.flatMap(this.apply(_, _transformators))
}
