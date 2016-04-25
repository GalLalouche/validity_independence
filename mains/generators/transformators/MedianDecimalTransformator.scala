package mains.generators.transformators

import common.rich.collections.RichTraversableDouble.richTraversableDouble

object MedianDecimalTransformator extends ComplexTransformator {
	override def apply(ds: Seq[Double]) = {
		val median = ds.median
		val decimal = ds.decimal
		val medianLocation = 0.5
		val decimalLocation = 0.1
		val a = (medianLocation - decimalLocation) / (median - decimal)
		assert(a != 0)
		val b = medianLocation - (a * median)
		ds map (a * _) map (_ + b	)
	}
	override def getLatexHead(name: String) = s"\\mu_{\\sfrac{1}{2}}-\\mu_{\\sfrac{1}{10}}"
}
