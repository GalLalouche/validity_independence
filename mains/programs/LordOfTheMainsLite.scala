
package mains.programs

import Jama.Matrix
import common.rich.RichAll._
import corpus._
import corpus.bugs.CorporaDataWithBugs
import corpus.rich.RichData._
import mains.generators.lists.MetricsDescriptor
import mains.generators.tables._
import mains.generators.tables.bugs._
import mains.generators.tables.pca.{PCAEigenValues, PCAAngles}
import mains.generators.transformators._
import metrics._
import stats._

/**
 * Same flavour as lord of the main, zero the calories!
 * Does not run "heavy computations" (e.g., parsing metrics)
 */
object LordOfTheMainsLite extends Debug {

	override def timedMain {
		val rawData = MetricValuesParser.getData.ignoreMetrics(Configuration.ignoredMetrics)
		metricsData(rawData.metrics)
		val averagedData: CorporaData = rawData.getAverageValuesPerFile
		correlations(averagedData)
		val pcaMatrix = calcPcaMatrix(averagedData.normalizedByRankings.concatenated)
//		val buggyData = Configuration.featureGenerator.load.ignoreMetrics(Configuration.ignoredMetrics)
		//		ktMetricDifference(buggyData, pcaMatrix)
//		pcaDistributionAcrossProjects(rawData, Configuration.namedNormalizations)
	}

	private def correlations(data: CorporaData) {
		def appendSuffix(suffix: String = "") = Seq(KendallTau, Pearson, Spearman)
			.map(new CorrelationTableGenerator(_))
			.map(_.withName(_ + suffix + "InterCorrelation"))

		appendSuffix().foreach(_.apply(data))
		val PCAed = IndividualPCATransformation.apply(data.normalizedByRankings)
		appendSuffix("PCA") foreach (_ apply PCAed)
		appendSuffix("RankNormalized") foreach (_ apply (SizeRankNormalizationTransformation apply data))
		appendSuffix("LinearNormalized") foreach (_ apply (SizeLinearNormalizationTransformation apply data))
	}

	private def calcPcaMatrix(data: CorpusData) = {
		PCA.writePCAMatrixToLatex(data.metricValuesOnly, data.metricNames)(PCA.file)
		PCA(data.metricValuesOnly)
	}

	private def pcaDistributionAcrossProjects(data: CorporaData, normalizations: Seq[LatexNamedTransformator]) {
		def mains(suffix: String = "") = Seq(
			PCAAngles.withName(_ + suffix)(_: CorporaData, normalizations),
			PCAEigenValues.withName(_ + suffix)(_: CorporaData, normalizations))
		mains().foreach(_(data))
		val normalizedData = SizeRankNormalizationTransformation apply data map (_.filterNot(_.m == NumberOfTokens))
		mains("Normalized").foreach(_(normalizedData))
	}

	private def ktMetricDifference(data: CorporaDataWithBugs[Double], pcaMatrix: Matrix) {
		KendallTauBugs(data)
		KendallTauBugsPCA(pcaMatrix)(data.normalizedByRankings)
		KendallTauBugsRankNormalized(data)
		KendallTauBugsLinearNormalized(data)
	}

	private def metricsData(metrics: Seq[Metric]) {
		MetricsDescriptor apply metrics
	}
}
