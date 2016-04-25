package stats

import org.apache.commons.math3.stat.inference.MannWhitneyUTest

object MannWhitneyCorrelation extends Correlation {
  private val mw = new MannWhitneyUTest
  override def apply(x: Seq[Double], y: Seq[Double]): Double = mw.mannWhitneyUTest(x toArray, y toArray)
}