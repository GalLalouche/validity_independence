package mains.generators.plots

import java.io.File
import common.rich.path.TempDirectory
import common.rich.path.RichFile
import mains.programs.{Configuration, MetricValuesParser}
import metrics.Monkey
import corpus.rich.RichData._
import org.ddahl.jvmr.RInScala
import common.rich.RichT._
import stats.RGetter

object ClusterAnalysis {
  def main(args: Array[String]) {
    val r = RGetter.get
    val d = TempDirectory()
    val data = MetricValuesParser.getData
      .getAverageValuesPerFile
      .normalizedByRankings
      .concatenated
      .ignoreMetrics(Set(Monkey))
    val tempCsvFile = d addFile "res.csv"
    tempCsvFile.appendLine(data.metricNames mkString ",")
    data.transpose.map(_.metricValues.map(e => e.metricValue)).foreach(l => tempCsvFile.appendLine(l mkString ","))
    val resFile: RichFile = Configuration.rawValuesFolder.parent addSubDir "Clusters" addFile "cluster.pdf"
    r.eval( s"""pdf("${resFile.path}")""".log())
    r.eval( s"""plot(hclust(dist(t(read.csv(file="${tempCsvFile.path}",head=TRUE))), method = "single"))""".log())
    r.eval("dev.off()".log())
  }
}
