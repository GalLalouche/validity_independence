package mains.generators.tables.bugs

import common.rich.CacheMap
import common.rich.RichAll._
import corpus.CorpusData
import corpus.bugs.{MetaCorrelation, CorporaDataWithBugs, Feature}
import corpus.rich.RichData._
import mains.generators.metricValues.{FeatureCache, FeatureParser}
import mains.generators.tables.LatexTableGenerator
import mains.generators.transformators._
import mains.programs.{Configuration, Debug}
import metrics.{Metric, NumberOfTokens}
import stats.{Correlation, KendallTau, PCA}
import stats.regression.LinearRegression

object ResidualsSignificance extends LatexTableGenerator with Debug {
	override val formatTableItem: PartialFunction[Any, Any] = {
		case d: Double if d < 0.01 => "0.00"
	}
	val pcaCache: CorpusData => Seq[Vector[Double]] = CacheMap[CorpusData, Seq[Vector[Double]]](PCA.getVectors)
	private val transformators = List(
		IdentityTransformator,
		SizeLinearNormalizationTransformation,
		SizeRankNormalizationTransformation
	)
	private def apply(f: Feature) {
		println("Feature " + f)
		val data = FeatureCache load f
		val (diffsPerTransformation, pValuesPerTransformation) = transformators.map(t => {
			val metricCoordinates = MetaCorrelation.apply(f, t, NumberOfTokens)
				.zipped
				.zip(data.metrics)
				.filterNot(_._2 == NumberOfTokens)
				.map(_.swap)
				.toMap
			val diffs = for ((m, (ys, xs)) <- metricCoordinates)
				yield m -> (ys - LinearRegression.linearRegression(metricCoordinates.values.toSeq).apply(xs))
			val (ck, other) = diffs
				.partition(e => Configuration.ckMetrics.contains(e._1))
				.map(_.values.toSeq.map(_.sq))
//			println(t + ": " + stats.TTest.apply(ck, other))
//			println(diffs map (e => e._1 -> e._2.withPrecision(4)))
//			println((ck.mean - other.mean) / (ck ++ other).standardDeviation)
			diffs -> stats.TTest.apply(ck, other)
		}).unzip
		val (conc1, conc2) = diffsPerTransformation
			.flatten
			.partition(e => Configuration.ckMetrics.contains(e._1))
			.map(_.map(_._2))
		writeLineToLatex((f.shortName :: pValuesPerTransformation) + stats.TTest.apply(conc1, conc2))
	}
	
	override def timedMain {
		Configuration.goodFeatures.foreach(apply)
	}
}
