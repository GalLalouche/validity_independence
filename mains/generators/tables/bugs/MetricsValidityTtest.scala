package mains.generators.tables.bugs

import common.TupleFunction._
import common.rich.RichAll._
import corpus.bugs.{CorporaDataWithBugs, Feature}
import corpus.rich.RichData._
import mains.generators.metricValues.{FeatureCache, FeatureParser}
import mains.generators.tables.{LatexTableWithHeadersGenerator, MatrixTableGenerator}
import mains.programs.{Configuration, Debug, MetricValuesParser}
import metrics._
import stats._

object MetricsValidityTtest extends MatrixTableGenerator[Feature, Metric] with Debug {
	val cache = FeatureCache

	private val sizeMetric = NumberOfTokens
	override def createCell(f: Feature, m: Metric): Any = {
		val data = for (c <- cache.load(f).corpora) yield {
			val bugs = c.bugs
			(KendallTau(c(m).metricValuesOnly, bugs), KendallTau(c(sizeMetric).metricValuesOnly, bugs))
		}
		data
			.map(e => e._1 / e._2)
			.mean.withPrecision(3)
			.mapIf(_.startsWith("1")).to(_.textbf)
			.mapTo(s => s"($s, ${PairedTTest.apply(data.unzip).significance})")
	}
	override protected val preTabular: Seq[String] = List("\\scriptsize")
	override protected val caption: String = "Student's t-tests for all metrics, checking if it is differentiable from \\NOT. " +
		"The blackness of the font measures its significance ($\\geq 0.05$, $\\leq 0.05$, $\\leq 0.01$, $\\leq 0.001$). Improvements, i.e. " +
		"ratios above 1.0, are bolded."
	override protected val alignments: String = "l*{30}{@{\\hspace{0pt}}c@{\\hspace{0pt}}}"
	//override this to provide your own headers!

	override def timedMain {
		val metrics = MetricValuesParser.getData.ignoreMetrics(Configuration.ignoredMetrics).metrics
		this.apply(Configuration.goodFeatures, metrics filterNot (_ == NumberOfTokens))
	}
	override def customHeaders(columns: Seq[Feature]): Seq[String] =
		columns.map(_.shortName.wrapInMacro("parbox{6em}"))
}
