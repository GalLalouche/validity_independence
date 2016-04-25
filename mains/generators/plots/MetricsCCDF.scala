package mains.generators.plots

import metrics.Metric
import mains.generators.tables.LatexTableGenerator
import common.rich.path.RichFile._

object MetricsCCDF extends LatexTableGenerator {

  def apply(t: Seq[Metric]) { apply(t, 3) }

  def apply(ms: Seq[Metric], tableWidth: Int) {
    ms
      .map(m => s"\\ccdensityGraph{${m.shortName }}")
      .grouped(tableWidth)
      .foreach(writeLineToLatex(_))
  }
}