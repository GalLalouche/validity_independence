package mains.generators.plots

import metrics.Metric
import mains.generators.LatexGenerator
import common.rich.path.RichFile._


object MetricsDensities extends LatexGenerator {
	def apply(xs: Seq[Metric]) {
    xs foreach (m => file appendLine s"\\densityGraph{${m.shortName}}")
  }
}
