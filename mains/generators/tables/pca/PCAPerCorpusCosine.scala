package mains.generators.tables.pca

import common.rich.RichAll.richTraversableDouble
import common.rich.path.RichPath.poorPath
import corpus.CorporaData
import corpus.rich.RichData.richCorporaData
import mains.generators.tables.FullTableGenerator
import mains.programs.{ Configuration, Debug, MetricValuesParser }
import metrics._
import stats.PCAMetric

object PCAPerCorpusCosine extends FullTableGenerator with Debug {
	val headers = Seq("Metric", "\\multicolumn{2}{c}{Mean Angle}", "\\multicolumn{2}{c}{Mean EigenValue}")
	val alignments = """|l
										 |S[table-format=5.1,round-precision=1,table-alignment=right]@{\textdegree$\pm$}S[table-format=2,round-precision=0,table-number-alignment=left]@{\%\hspace{2em}}
										 |S[table-format=2.1e-1,round-mode=off]@{$\pm$}S[table-format=2,table-number-alignment=left,round-precision=0]@{\%\hspace{0pt}}""".stripMargin
	override protected def timedMain {
		apply(MetricValuesParser.getData.withoutTests.filterLatest)
	}

	def apply(data: CorporaData) {
		createTable(data, Configuration.softwareMetrics.toSet)
		createTable(data, Configuration.ckMetrics, "Ck")
	}

	private def createTable(rawData: CorporaData, metricsToUse: Set[Metric], nameSuffix: String = "") {
		val stdToPercentage: (((Double, Double)) => Double) = e => e._2 * 100 / e._1
		val data = rawData
			.ignoreMetrics(BranchCounter, NumberOfCommentCharacters, McCabesComplexity)
			.useMetrics(metricsToUse)
			.getAverageValuesPerFile
			.normalizedByRankings
		val summarizer: (Traversable[Double]) => (Double, Double) = e => e.mean -> e.standardDeviation
		val angles = PCAAngles.extractDataForNormalization(data).map(summarizer).toList
		val eigenValues = PCAEigenValues.extractDataForNormalization(data).map(summarizer).toList
		val body = List(PCAMetric.generateMetrics(data.metrics.size).map(_.shortName),
			angles.map(_._1),
			angles.map(stdToPercentage),
			eigenValues.map(_._1),
			eigenValues.map(stdToPercentage)
		).transpose
		val file = new java.io.File(this.file.parent, s"${this.file.nameWithoutExtension}$nameSuffix.${this.file.extension}")
		file.createNewFile()
		bodyOnly.writeLinesToLatex(body, file)
	}
	override val caption: String = "Consistency of Principle Component vectors across the corpora"
	override protected val formatTableItem: PartialFunction[Any, Any] = {
		case d: Double if d >= 100 && d <= 10000 => f"$d%.2f" // allow three digits
	}
}
