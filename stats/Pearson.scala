package stats

import org.apache.commons.math3.stat.correlation.PearsonsCorrelation

object Pearson extends Correlation {
  override def apply(x: Seq[Double], y: Seq[Double]): Double = {
    new PearsonsCorrelation().correlation(x toArray, y toArray)
  }
}