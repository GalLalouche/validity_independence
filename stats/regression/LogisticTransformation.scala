package stats.regression
import common.rich.primitives.RichDouble._
case class LogisticTransformation(intercept: Double, slope: Double) extends RegressionTransformation {
	override def toString = s"${intercept.withPrecision(4)} + ${slope.withPrecision(4)}x"
	private val linear = LinearTransformation(intercept = intercept, slope = slope)
	def apply(x: Double) = 1 / (1 + Math.exp(-1 * linear(x)))
}