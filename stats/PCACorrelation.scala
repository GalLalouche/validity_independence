package stats

import Jama.Matrix
import stats.RichMatrix._

class PCACorrelation(c: Correlation) {
  def apply(pcaMatrix: Matrix) = new Correlation {
    override def apply(v1: Seq[Double], v2: Seq[Double]) = c.apply(v1, v2)
    override def applyAutoCorrelation(data: Seq[Seq[Double]]) =
      super.applyAutoCorrelation((pcaMatrix * RichMatrix(data)).deepSeq)
  }
}