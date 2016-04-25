package mains.generators.tables.old

import corpus.bugs._
import corpus.rich.RichCorpusData._
import mains.generators.tables.LatexTableGenerator

trait LatexTestTableMaker extends LatexTableGenerator {

  protected def calculateValueForMetric(data: MetricDataWithBugs[Boolean]): Double

  def calculateTestScorePerMetric(data: CorpusDataWithBugs[Boolean]) =
    file.appendLine(
      (data.corpusName :: data.metricValues.map(calculateValueForMetric).toList).toLatexSimple)

  def apply(data: CorporaDataWithBugs[Boolean]) {
    data.corpora foreach calculateTestScorePerMetric
  }
}
