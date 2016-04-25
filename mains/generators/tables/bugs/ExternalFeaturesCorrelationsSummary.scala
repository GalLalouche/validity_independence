package mains.generators.tables.bugs

import common.rich.RichAll._
import corpus.CorporaData
import corpus.rich.RichData._
import mains.generators.metricValues.FeatureCache
import mains.generators.tables.LatexTableGenerator
import mains.programs.{Configuration, Debug, MetricValuesParser}
import metrics.{Metric, NumberOfTokens}
import stats.{Spearman, Correlation, KendallTau}

class ExternalFeaturesCorrelationsSummary(correlation: Correlation) extends LatexTableGenerator {
	protected override val formatTableItem: PartialFunction[Any, Any] = {
		case d: Double if d < 0.01 => "0.00"
	}

	override def fileName: String = correlation + super.fileName
	case class ValueForFeature(mean: Double, std: Double)
	case class Row(m: Metric, values: List[ValueForFeature], correlationWithNot: Double) {
		def write() {
			writeLineToLatex((m :: values.flatMap(e => List(e.mean, e.std))) :+ correlationWithNot)
		}
	}

	def apply() {
		val rawData: CorporaData = MetricValuesParser.getData
		val metrics = rawData.metrics
		for (m <- metrics) {
			val vs = for (f <- Configuration.goodFeatures) yield {
				val $: Seq[Double] = FeatureCache.load(f).corpora.map(c => correlation(c.apply(m).metricValuesOnly, c.bugs))
				ValueForFeature($.mean, $.standardDeviation)
			}
			val correlationWithNot = rawData.corpora.map(c => KendallTau(c.apply(m).metricValuesOnly, c.apply(NumberOfTokens).metricValuesOnly)).mean
			Row(m, vs.toList, correlationWithNot).write()
		}
	}
}

object ExternalFeaturesCorrelationsSummary extends Debug {
	override def timedMain {
		for (c <- List(KendallTau, Spearman))
			new ExternalFeaturesCorrelationsSummary(c).apply()
	}
}
