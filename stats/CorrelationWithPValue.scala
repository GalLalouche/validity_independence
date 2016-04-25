package stats

trait CorrelationWithPValue extends Correlation {
  /** Calculates the correlation and its matching p-value
   *  @return a tuple of the (correlation, p-value)
   */
	def withPValue(x: Seq[Double], y: Seq[Double]): (Double, Double)
	
	override def apply(xs: Seq[Double], ys: Seq[Double]): Double = withPValue(xs, ys)._1
	def pValue(xs: Seq[Double], ys: Seq[Double]) = withPValue(xs, ys)._2
}