package mains.generators.tables.bugs

import Jama.Matrix

object MeanMetricDifferenceOnBugs extends MeanMetricDifferenceAgainstBugs

object MeanMetricDifferenceOnBugsPCA {
  def apply(_pcaMatrix: Matrix) = new MeanMetricDifferenceAgainstBugs with PCABugCorrelation[Boolean] {
    override protected val pcaMatrix: Matrix = _pcaMatrix
    override def fileName: String = "MeanMetricDifferenceOnBugsPCA"
  }
}

object MeanMetricDifferenceOnBugsNormalized
  extends MeanMetricDifferenceAgainstBugs with RankNormalizedBugCorrelation[Boolean]




