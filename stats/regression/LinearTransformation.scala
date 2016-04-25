package stats.regression

import java.awt.geom.Line2D

import common.rich.primitives.RichDouble._

case class LinearTransformation(intercept: Double, slope: Double) extends RegressionTransformation {
	override def toString = s"${intercept.withPrecision(4)} + ${slope.withPrecision(4)}x"
	def apply(x: Double) = intercept + slope * x
	def logistic(x: Double) = 1 / (1 + Math.exp(-1 * apply(x)))
	def line2d = new Line2D.Double(0, apply(0), 1, apply(1))
}
