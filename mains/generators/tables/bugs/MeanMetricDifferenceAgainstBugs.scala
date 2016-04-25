package mains.generators.tables.bugs

import common.rich.RichAll._
import corpus.bugs._
import mains.programs.Debug

private[bugs] abstract class MeanMetricDifferenceAgainstBugs extends LatexBugCorrelationGenerator[Boolean] with Debug {

  override protected def apply(data: MetricDataWithBugs[Boolean]): Double =
    data
      .metricValues
      .mapTo(e => e.map(_.metricValue).normalizedByRankings zip e.map(_.bugValue))
      .partition(_._2) // split to buggy and non-buggy
      .map(_.map(_ _1))
      .map(e => e.mean)
      .mapTo(e => e._1 - e._2) * // get the mean difference
      100
}
