package mains.generators.tables.old

import common.rich.RichAll.richTuple
import corpus.bugs.MetricDataWithBugs
import org.apache.commons.math3.stat.inference.ChiSquareTest

class ChiSquareTestTable extends LatexTestTableMaker {

  private lazy val numberOfGroups = 10

  override def calculateValueForMetric(data: MetricDataWithBugs[Boolean]): Double = {
    val groupSize = data.metricValues.size / numberOfGroups
    val buggyAndNonBuggy = data.metricValues
      .toVector
      .sortBy(_.metricValue)
      .grouped(groupSize)
      .toVector
      .map(_.partition(_.bugValue).map(_.size.toLong))
      .unzip
    val asPercentage = buggyAndNonBuggy.map(_.map(e ⇒ f"${(e.toDouble / groupSize) * 100}%05.2f"))
    new ChiSquareTest().chiSquareTest(Array(buggyAndNonBuggy._1 toArray, buggyAndNonBuggy._2 toArray))
  }

}

object ChiSquareMain extends ChiSquareTestTable
object ChiSquarePerMethod extends ChiSquareTestTable
