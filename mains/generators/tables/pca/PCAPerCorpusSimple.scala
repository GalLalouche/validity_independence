package mains.generators.tables.pca

import common.rich.RichAll._
import corpus.rich.RichData._
import mains.generators.tables.FullTableGenerator
import mains.programs.{Debug, MetricValuesParser}
import metrics.BranchCounter
import stats.PCAMetric


object PCAPerCorpusSimple extends FullTableGenerator with Debug {
  val headers = Seq("Metric", "\\multicolumn{2}{c}{Mean Angle}", "\\multicolumn{2}{c}{Mean EigenValue}")
  val alignments =
    """|l
     	 		|r@{$\pm$}S[table-format=2.2,table-number-alignment=left]@{\%\hspace{2em}}
     	 		|r@{$\pm$}S[table-format=2,table-number-alignment=left]<{\hspace{-1em}\%}""".stripMargin
  override protected def timedMain {
    // normalization by ranking is necessary, otherwise ETF pwns all
    val data = MetricValuesParser.getData.ignoreMetrics(BranchCounter).getAverageValuesPerFile.normalizedByRankings
    val summarizer: (Traversable[Double]) => (Double, Double) = e => (e.mean, e.standardDeviation)
    val angles = PCAAngles.extractDataForNormalization(data).map(summarizer).toList
    val eigenValues = PCAEigenValues.extractDataForNormalization(data).map(summarizer).toList
    val body = List(PCAMetric.generateMetrics(data.metrics.size).map(_.shortName),
      angles.map(_._1),
      angles.map(e => e._2 * 100 / e._1),
      eigenValues.map(_._1),
      eigenValues.map(e => e._2 * 100 / e._1)).transpose
        .take(10)
    writeLinesToLatex(body)
  }
  override val caption: String = "Consistency of Principle Component vectors across the corpora"
  override protected val formatTableItem: PartialFunction[Any, Any] = {
    case d: Double if d >= 100 && d <= 10000 => f"$d%.2f" // allow three digits
  }
}
