package mains.generators.tables.old

import common.rich.RichTuple.richTuple
import corpus.bugs.MetricDataWithBugs
import org.apache.commons.math3.stat.inference.MannWhitneyUTest

class MannWhitneyTestTable extends LatexTestTableMaker {

  override def calculateValueForMetric(data: MetricDataWithBugs[Boolean]): Double = {
    val buggyAndNonBuggy = data.metricValues
      .toVector
      .sortBy(_.metricValue)
      .partition(_.bugValue)
      .map(_.map(_.metricValue))
    if (buggyAndNonBuggy._1.isEmpty || buggyAndNonBuggy._2.isEmpty)
      1.0
    else
      new MannWhitneyUTest().mannWhitneyUTest(buggyAndNonBuggy._1 toArray, buggyAndNonBuggy._2 toArray)
  }
}

object MannWhitneyMain extends MannWhitneyTestTable
object MannWhitneyPerMethod extends MannWhitneyTestTable
