package mains.generators.transformators.permetric

import common.rich.collections.RichTraversableDouble.richTraversableDouble
import common.rich.primitives.RichDouble.richDouble
import corpus.CorporaData
import corpus.rich.RichData._

/**
 * Performs a linear transformation on the data, such that the median and decimal
 * location are in fixed locations
 */
object MedianDecimalGlobalTransformator extends PerMetricTransformator {
	private val medianLocation = 0.5
	private val decimalLocation = 0.1

	protected def applyForSingularMetric(data: CorporaData): CorporaData = {
		val flattened = data.metricValuesOnly.flatten.flatten
		val median = flattened.median
		val decimal = flattened.decimal
		if (median == decimal)
			return data
		val a = (medianLocation - decimalLocation) / (median - decimal)
		assert(a != 0)
		val b = medianLocation - (a * median)
		val $ = data map (_ map (_ map (x => a * x + b)))
		val newFlattened = $.metricValuesOnly.flatten.flatten
		assert(newFlattened.median.isRoughly(medianLocation))
		assert(newFlattened.decimal.isRoughly(decimalLocation))
		$
	}
}
