package stats

object KendallTau extends CorrelationWithPValue {
	def absolute = new Correlation {
		override def apply(x: Seq[Double], y: Seq[Double]): Double = Math.abs(KendallTau.this.apply(x, y))
	}

	override def withPValue(x: Seq[Double], y: Seq[Double]): (Double, Double) = {
		val $ = new KendallsCorrelation().correlation(x.toArray, y.toArray)
		($.getFirst, $.getSecond)
	}
}
