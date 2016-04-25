package mains.generators.tables.bugs

import java.io.File

import common.rich.RichAll._
import common.rich.path.Directory
import corpus._
import corpus.bugs.{CorporaDataWithBugs, Feature}
import corpus.rich.RichData._
import mains.generators.metricValues.FeatureParser
import mains.generators.transformators._
import mains.programs.{Configuration, Debug, MetricValuesParser}
import metrics.Metric
import stats.KendallTau

private[bugs] class FeatureCorrelationPerMetric(f: Feature, protected val _t: CorpusTransformator) {
	val tableGenerator = new NumericalBugCorrelation(KendallTau) with TransformationBugCorrelation[Double] {

		override protected def createMeanPerMetric: MeanPerMetric = new MeanPerMetric {

			override def fileName = super.fileName.replaceAll("anon.*", "MeanPerMetric")
			override def write() {
				val (metrics, xss) = seqs.unzip
				xss.foreach(xs => {
					assert(xs.last.isInstanceOf[Int])
					assert(xs.dropRight(1).forall(_.isInstanceOf[Double]))
				})
				val nots = xss.map(_.last.asInstanceOf[Int])
				val statistics = xss.map(_.dropRight(1).map(_.asInstanceOf[Double])).transpose
				val maxes = statistics.map(_.map(Math.abs).max.ceil.toInt) :+ nots.map(Math.abs).max
				val numbersAsStrings = xss.transpose.map(_.map {
					case d: Double => d.withPrecision(2)
					case i: Int => i.toString
				})
				val $ = maxes
					.zip(numbersAsStrings)
					.map { case (max, xs) => xs.map(_.wrapInMacro(s"ApplyLocalGradient{$max}"))}
					.transpose
				writeTable(None, metrics.zip($).map(e => e._1.shortName :: e._2.toList))
			}
		}
		override protected lazy val rawData: CorporaData = FeatureCorrelationPerMetric.this.rawData
		override protected def t: CorpusTransformator = _t
		override def directory: Directory = super.directory
			.addSubDir("Bugs")
			.addSubDir(FeatureCorrelationPerMetric.toHumanReadable(_t))
		override def fileName: String = f.toString.replaceAll(" ", "")
	}
	protected lazy val rawData: CorporaData = FeatureCorrelationPerMetric.transformatorsCache(_t)

	def apply(data: CorporaDataWithBugs[Double]) {
		tableGenerator.apply(data)
	}
}

object FeatureCorrelationPerMetric extends Debug {
	private val rawData = MetricValuesParser.getData.ignoreMetrics(Configuration.ignoredMetrics).getAverageValuesPerFile.normalizedByRankings
	private val pcaTransformation = IndividualPCATransformation
	private lazy val rawPCAData = timed("Creating PCA cache") {pcaTransformation(rawData)}
	private def toHumanReadable(e: CorpusTransformator) = e match {
		case IdentityTransformator => "Identity"
		case SizeRankNormalizationTransformation => "Rank"
		case SizeLinearNormalizationTransformation => "Linear"
		case IndividualPCATransformation => "PCA"
		case _: PCATransformation => "PCA"
	}
	private lazy val (transformators, transformatorsCache) = timed("Creating transformations cache") {
		val $ = List(IdentityTransformator,
			SizeLinearNormalizationTransformation,
			SizeRankNormalizationTransformation)
		$ -> {
			val data = MetricValuesParser.getData
			$.map(e => {
				// why the frak is is Map invariant in type A?!
				e.asInstanceOf[CorpusTransformator] -> (e apply data).getAverageValuesPerFile
			}).toMap
		}
	}
	private class FeatureCorrelationPerMetricPCA(f: Feature) extends FeatureCorrelationPerMetric(f, pcaTransformation) {
		override protected lazy val rawData: CorporaData = rawPCAData
	}
	override protected def timedMain {
		for (f <- Configuration.features) {
			val featureData = FeatureParser load f
			for (t <- transformators)
				new FeatureCorrelationPerMetric(f, t).apply(featureData)
			new FeatureCorrelationPerMetricPCA(f).apply(featureData.normalizedByRankings)
		}
	}
}
