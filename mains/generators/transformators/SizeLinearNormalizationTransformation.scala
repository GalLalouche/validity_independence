package mains.generators.transformators

import corpus.CorpusData
import metrics.{Metric, NumberOfTokens}

class SizeLinearNormalizationTransformation(sizeMetric: Metric) extends SizeNormalizationTransformation(sizeMetric) {
  override protected val suffix: String = "L"
  override protected def modify(data: CorpusData): CorpusData = data
}

object SizeLinearNormalizationTransformation extends SizeLinearNormalizationTransformation(NumberOfTokens)
