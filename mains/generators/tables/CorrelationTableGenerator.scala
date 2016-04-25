package mains.generators.tables

import Jama.Matrix
import common.rich.RichAll._
import corpus.rich.RichData._
import corpus.{CorporaData, CorpusData, MetricData}
import metrics.post.{DepthOfInheritance, NumberOfChildren}
import metrics._
import stats.Correlation
import stats.RichMatrix._

class CorrelationTableGenerator(c: Correlation) {
  val fileName: String = c.simpleName
  def apply(data: CorpusData) {
    writeMatrix(c.applyAutoCorrelation(data.metricValuesOnly).deepSeq, data.metrics)
  }

  private def writeMatrix(data: Seq[Seq[Double]], metrics: Seq[Metric]) {
    new LatexTableGenerator {
      override protected val formatTableItem: PartialFunction[Any, Any] = {
        case x: Double => f"$x%.2f".wrapInMacro("g")
        case x: Int => x.toString.wrapInMacro("g")
        case s: String => s.wrapInMacro("h")
      }
      override def fileName: String = CorrelationTableGenerator.this.fileName

      def apply() {
        for ((m, data) <- metrics zip data) yield {
          if (m == NumberOfTokens)
            file appendLine ("\\hline")
          writeLineToLatex(m.shortName :: data.map(_.toInt))
          if (m == NumberOfTokens)
            file appendLine ("\\hline")
        }
      }
    }.apply()
  }
  def sortByForMetric(m: Metric) = {
    val group: Int = m match {
      case Coupling | LackOfCohesion | DepthOfInheritance | NumberOfChildren | ResponsesForAClass | WMC => 0
      case MethodCounter | FieldCounter | ExplicitMutability | ConstructorCounter | Chameleonicity => 1
      case LinesOfCode | NumberOfTokens | StatementCounter => 2
      case NumberOfAlphabeticalCommentCharacters | NumberOfCommentCharacters => 3
      case LempelZivRegularity | GZipRegularity | EntropyRegularity => 4
      case McCabesComplexity | McCabesWithShortCircuit | LoopCounter | HorizontalComplexity => 5
      case EulerTotient | Monkey => 6
      case e if e.toString().contains("SHA") => 6
    }
    group -> m.shortName
  }
  def sortMetrics(metrics: Seq[MetricData]): Seq[MetricData] = metrics sortBy (_.m.mapTo { m => sortByForMetric(m) -> m.shortName })
  def apply(data: CorporaData) {
    val dataWithSortedMetrics = data.corpora
        .map(_.sortMetrics(sortMetrics))
    val correlationMatrix = dataWithSortedMetrics
        .map(_.metricValuesOnly)
        .map(c.applyAutoCorrelation)
        .map(new Matrix(_))
        .reduce(_ + _)
        .mapCells(_ / 0.26)
    writeMatrix(correlationMatrix.deepSeq, data.metrics)
  }

  def withName(f: String => String) = new CorrelationTableGenerator(c) {
    override val fileName: String = f(CorrelationTableGenerator.this.fileName)
  }
}
