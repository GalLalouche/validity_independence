package mains.generators.transformators

import corpus.CorpusData
import corpus.rich.RichCorpusData.richCorpusData
import metrics.{Metric, NumberOfTokens}

// normalizes by the rank normalization, then divides by the rank of NOT
class SizeRankNormalizationTransformation(sizeMetric: Metric) extends SizeNormalizationTransformation(sizeMetric) {
  override protected val suffix: String = "R"

  override protected def modify(data: CorpusData): CorpusData = data.normalizedByRankings
  override def getLatexHead(variableName: String): String = s"\\frac{rank($variableName)}{rank(\\text{${sizeMetric.shortName })}"
}

object SizeRankNormalizationTransformation extends SizeRankNormalizationTransformation(NumberOfTokens)
