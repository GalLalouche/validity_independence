package metrics

// same as Metric, but the normalization is inversed
trait InverseMetric extends Metric {
//override def normalizeValue(value: Double, numberOfTokens: Double): Double = 1. / super.normalizeValue(value, numberOfTokens)
}