
package mains.programs

import corpus.CorporaData
import mains.generators.plots.MeanBugsToSize
import mains.generators.tables.CorrelationTableGenerator
import mains.generators.tables.bugs._
import mains.generators.tables.dataset.DatasetFeatures
import mains.generators.transformators.{SizeLinearNormalizationTransformation, SizeRankNormalizationTransformation}
import stats.KendallTau

/**
 * Same flavour as lord of the main, zero the calories!
 * Does not run "heavy computations" (e.g., parsing metrics)
 */
object LordOfTheMainsEmse extends Debug {

	override def timedMain {
//		KendallTauBugsSummary.timedMain
//		DatasetFeatures.timedMain
//		ResidualsSignificance.timedMain
//		MetaCorrelationRSquared.timedMain
//		MeanBugsToSize.timedMain
		correlations(MetricValuesParser.getData)
	}

	private def correlations(data: CorporaData) {
		def appendSuffix(suffix: String = "") = new CorrelationTableGenerator(KendallTau).withName(_ + suffix + "InterCorrelation")
		appendSuffix().apply(data)
		appendSuffix("RankNormalized").apply(SizeRankNormalizationTransformation apply data)
		appendSuffix("LinearNormalized").apply(SizeLinearNormalizationTransformation apply data)
	}
}
