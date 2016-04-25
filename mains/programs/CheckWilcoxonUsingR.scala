package mains.programs

import corpus.rich.RichData._
import org.ddahl.jvmr.RInScala
import common.rich.RichAll._
import scala.util.Random

object CheckWilcoxonUsingR extends Debug {
  override def timedMain {
    val r = RInScala( """C:\dev\lang\R\R-3.0.2\bin\x64\R.exe""")
    val data = MetricValuesParser.getData.getAverageValuesPerFile
    (for (corpusData <- data.corpora;
          metricData <- corpusData.metricValues) yield {
      val metricValues: Seq[Double] = metricData.metricValuesOnly.mapTo {
        case e if e.size > 5000 => e.shuffle.take(100)
        case e => e
      }.rerange2Positives.map(Math.log)
      val p = r.capture(s"shapiro.test(${metricValues.mkString("c(", ",", ")") })").split("\n")
        .last
        .captureWith( """.*?p-value [<=] (.*?)""".r)
      if (p.toDouble > 0.05)
        Some(corpusData.c, metricData.m, p)
      else
        None
    }).defined
      .groupBy(_._2).foreach { e =>
      val m = e._1
      println(s"Metric $m is log normal in the following projects: " + e._2.map(x => (x._1, x._3)).mkString(", "))
    }
  }
}