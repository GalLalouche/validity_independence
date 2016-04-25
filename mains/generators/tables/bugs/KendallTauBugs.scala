package mains.generators.tables.bugs

import Jama.Matrix
import stats.KendallTau

object KendallTauBugs extends NumericalBugCorrelation(KendallTau)

object KendallTauBugsPCA {
	def apply(_pcaMatrix: Matrix) = new NumericalBugCorrelation(KendallTau) with PCABugCorrelation[Double] {
		override protected val pcaMatrix: Matrix = _pcaMatrix
		override def fileName: String = "KendallTauBugsPCA"
	}
}

object KendallTauBugsRankNormalized extends NumericalBugCorrelation(KendallTau) with RankNormalizedBugCorrelation[Double]
object KendallTauBugsLinearNormalized extends NumericalBugCorrelation(KendallTau) with LinearNormalizedBugCorrelation[Double]
