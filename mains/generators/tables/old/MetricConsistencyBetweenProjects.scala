package mains.generators.tables.old

import common.rich.RichAll._
import corpus._
import corpus.rich.RichData._
import mains.generators.tables.LatexTableWithDynamicHeaders
import mains.generators.transformators.LatexNamedTransformator

trait MetricConsistencyBetweenProjects extends LatexTableWithDynamicHeaders {
	protected val correlation: (Seq[Double], Seq[Double]) => Double

	protected val baseSignificance: Double

	override protected val formatTableItem: PartialFunction[Any, Any] = {
		case e: Double => f"${(e * 100).toInt}%d".wrapInMacro("applyBold")
	}

	def apply(t: (CorporaData, Seq[LatexNamedTransformator])) {
		val (data, normalizations) = t
		val headers = ("Metric" :: "Entropy" :: normalizations).map(_.toString.align(center))
		writeTable(Some(headers),
			calculateConsistency(data, normalizations),
			calculateStatistics(calculateConsistency(data, normalizations)))
	}

	def apply(data: CorporaData, normalizations: Seq[LatexNamedTransformator]) { apply((data, normalizations)) }

	private def calculateStatistics(body: Seq[Seq[Any]]): Seq[Seq[Any]] = {
		val statistics = Vector[(String, Seq[Double] => Double)](
			("Mean", e => e.mean),
			("$\\sigma$", e => e.standardDeviation),
			("Median", e => e.median),
			("MAD", e => e.medianAbsoluteDeviation),
			("Min", e => e.min),
			("Max", e => e.max)
		).map(e => e._1.textbf.align(right) -> e._2)
		val dataPerNormalization = body
			.map(_ drop 2 map (_.toString.toDouble))
			.transpose
			.map(xs => statistics map (_._2 apply xs))
			.transpose
		statistics map (_._1) zip dataPerNormalization map (e => "" :: e._1 :: e._2)
	}

	private def calculateConsistency(data: CorporaData, normalizations: Seq[LatexNamedTransformator]): Seq[Seq[Any]] = {
		val entropyPerMetric = data.concatenated
			.metricValues
			.map(dfm => dfm.m -> dfm.metricValuesOnly.entropy)
			.toMap
		for (
			(m, dataPerNormalization) <- data.metrics zip (for (n <- normalizations.par) yield {
				for (m <- data.metrics) yield { 
					for ((c1, c2) <- n.apply(data).corpora.getUniquePairs) yield {
						correlation(c1(m).metricValuesOnly, c2(m).metricValuesOnly)
					}
				}.percentageSatisfying(_ > baseSignificance)
			}).transpose
		) yield ("\\" + m.shortName) :: f"${entropyPerMetric(m)}%2.2f" :: dataPerNormalization.seq
	}

	def withTableName(name: String) = new MetricConsistencyBetweenProjects {
		override val fileName = name
		override val baseSignificance = MetricConsistencyBetweenProjects.this.baseSignificance
		override val correlation = MetricConsistencyBetweenProjects.this.correlation

	}

	def withTableName(f: String => String): MetricConsistencyBetweenProjects = withTableName(f(fileName))
}
