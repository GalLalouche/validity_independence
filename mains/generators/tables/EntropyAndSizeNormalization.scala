package mains.generators.tables

import common.rich.RichAll._
import corpus.rich.RichData._
import mains.generators.transformators.{SizeLinearNormalizationTransformation, SizeRankNormalizationTransformation}
import mains.programs.{MetricValuesParser, Debug}
import metrics._

object EntropyAndSizeNormalization extends LatexTableWithDynamicHeaders with Debug {

	override protected val formatTableItem: PartialFunction[Any, Any] = {
		case d: Double => d.withPrecision(2)
	}

	override protected def timedMain {
		val data = MetricValuesParser.getData
		val rankedData = SizeRankNormalizationTransformation apply data
		val linearData = SizeLinearNormalizationTransformation apply data
		val body = for (m <- data.metrics) yield
			List(m.shortName,
				data.corpora.map(_(m).metricValuesOnly.entropy).mean,
				data.correlationBetween(m, NumberOfTokens),
				linearData.correlationBetween(m, NumberOfTokens),
				rankedData.correlationBetween(m, NumberOfTokens))
		writeTable(None, body)
	}
}
