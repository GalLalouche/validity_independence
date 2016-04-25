package mains.generators.tables.dataset

import java.awt.Font
import java.util

import corpus.bugs.Feature
import common.rich.RichAll._
import corpus.bugs.Feature.{AccumulativeIsBugInComment, AverageCommitsUntilNextChange, AverageDifferenceSize, MedianCommitsUntilNextChange}
import mains.generators.metricValues.FeatureCache
import mains.programs.Debug
import org.jfree.chart.{ChartPanel, JFreeChart}
import org.jfree.chart.axis.{CategoryAxis, NumberAxis}
import org.jfree.chart.plot.CategoryPlot
import org.jfree.chart.renderer.category.BoxAndWhiskerRenderer
import org.jfree.data.general.{DatasetChangeListener, DatasetGroup}
import org.jfree.data.statistics.{BoxAndWhiskerItem, BoxAndWhiskerXYDataset, DefaultBoxAndWhiskerCategoryDataset}
import org.sameersingh.scalaplot.jfreegraph.JFGraphPlotter

object FeaturesBoxplots extends Debug {

  case class BoxPlotItem(mean: Double, median: Double, q1: Double, q3: Double, minRegularValue: Double, maxRegularValue: Double,
      minOutlier: Double, maxOutlier: Double) {
    def toBAWItem: BoxAndWhiskerItem = new BoxAndWhiskerItem(mean, median, q1, q3, minRegularValue, maxRegularValue, minOutlier, maxOutlier,
      new util.LinkedList[Object]())
  }
  object BoxPlotItem {
    def apply(xs: Seq[Double]): BoxPlotItem = {
      new BoxPlotItem(mean = xs.mean, median = xs.median, q1 = xs percentile 0.25, q3 = xs percentile 0.75,
        minRegularValue = xs percentile 0.15, maxRegularValue = xs percentile 0.85, xs percentile 0.10, xs percentile 0.9)
    }
  }
  def apply(f: Feature) {
    val boxPlots = FeatureCache.load(f).corpora
        .map(_.bugs)
        .map(BoxPlotItem.apply)
        .map(_.toBAWItem)
    val dataset = new DefaultBoxAndWhiskerCategoryDataset()
    for ((b, c) <- boxPlots.zip('A' to 'Z'))
      dataset.add(b, c, 0)
    val ca = new CategoryAxis("Corpora")
    val na = new NumberAxis("Feature values")
    val renderer = new BoxAndWhiskerRenderer
    renderer.setFillBox(true)
    val plot = new CategoryPlot(dataset, ca, na, renderer)
    val chart = new JFreeChart("demo", new Font("SansSerif", Font.BOLD, 14), plot, true)
    JFGraphPlotter gui chart
  }

  override protected def timedMain {
    apply(AverageDifferenceSize)
  }
}
