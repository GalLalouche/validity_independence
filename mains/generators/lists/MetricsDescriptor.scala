package mains.generators.lists

import mains.generators.plots.MetricsCCDF
import mains.programs.Configuration
import metrics.Metric
import org.json4s._

object MetricsDescriptor extends JsonListReader[Metric]("Metrics") {
  override def nameExtractor(t: Metric): String = t.shortName
  protected def getLabel(t: Metric, js: JValue): String = 
    js.findField({ case (k, v) => k == "shortName"})
			.map(_._2.values.toString)
			.getOrElse(t.shortName)
			.textbf
  protected def getContent(js: JValue): String = s"\\emph{${js \ "name" values}.} ${js \ "desc" values }"

  def timedMain(args: Array[String]) {
    val metrics = Configuration.softwareMetrics.filterNot(Configuration.ignoredMetrics.contains)
    apply(metrics)
  }
}