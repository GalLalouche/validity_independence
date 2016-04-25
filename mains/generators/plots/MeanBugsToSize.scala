package mains.generators.plots

import java.awt.{Color, Font}

import common.rich.RichAll._
import corpus.bugs._
import corpus.rich.RichData._
import jsc.swt.virtualgraphics.Ellipse
import mains.generators.metricValues.FeatureCache
import mains.generators.transformators._
import mains.programs._
import metrics._
import metrics.post.{DepthOfInheritance, NumberOfChildren}
import org.jfree.chart.plot.XYPlot
import org.jfree.util.ShapeUtilities
import plots.{Plot, RegressionBuilderDecorator, ScatterPlotBuilder}

class MeanBugsToSize(sizeMetric: Metric, featureName: String) {
  val plotBuilder = MeanBugsToSize.plotBuilder
      .nameY(s"Correlation to $featureName")
      .nameX(s"Correlation to " + sizeMetric)
      .title(s"MetaCorrelations of $sizeMetric and $featureName").mapTo(new RegressionBuilderDecorator(_) {
    override val regressionName: String = "Global Regression"
  })

  private val transformators = List(
    IdentityTransformator,
    SizeLinearNormalizationTransformation,
    SizeRankNormalizationTransformation
  )

  def apply(f: Feature) {
    val dir = Plot.savedDirectory \ "MetaCorrelation" \ s"${sizeMetric.shortName.toPascalCase }"
    dir.mkdirs()
    transformators
        .map(t => MeanBugsToSize.toHumanReadable(t) -> MetaCorrelation.apply(f, t, sizeMetric))
        .foldLeft(plotBuilder) { case (pb, (tName, data)) =>
          val metrics = FeatureCache.load(f).metrics
          val (ck, nonCk) = metrics zip data.zipped partition (e => Configuration.ckMetrics(e._1)) map (_.map(_._2))
          pb.addSeries(nonCk, s"$tName")
              .addSeries(ck, s"$tName (C\\&K)")
        }.buildWithRegression
//        .show
        .save(dir \ s"${sizeMetric.shortName.toPascalCase }$featureName.pdf".replaceAll(" ", ""))
  }
}

object MeanBugsToSize extends Debug {
  private val plotBuilder = new ScatterPlotBuilder {
    override def getRenderer = {
      val $ = new org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
      $.setBaseLinesVisible(false)
      $.setDrawOutlines(false)
      $.setUseOutlinePaint(true)
      $.setUseFillPaint(true)
      $.setDrawOutlines(true)

      val radius = 8
      val shapes = Seq(new Ellipse(0, 0, radius, radius), ShapeUtilities.createUpTriangle(radius / 2), ShapeUtilities.createDiamond(radius / 2))
      val colors = List(Color.red, Color.blue, Color.green)
      for ((s, i, c) <- shapes.zipWithIndex flatZip colors) {
        val index = 2 * i
        $.setSeriesFillPaint(index, c)
        $.setSeriesOutlinePaint(index, c)
        val ckIndex: Int = index + 1
        $.setSeriesShape(ckIndex, s)
        $.setSeriesOutlinePaint(ckIndex, c)
        $.setSeriesVisibleInLegend(ckIndex, false)
        $.setSeriesFillPaint(ckIndex, Color.white)
      }
      $
    }
  }.height(600).width(800).title("Template...").appendChartModifier(c => {
    c.getXYPlot.getRangeAxis.setRange(-1, 1)
    c.getXYPlot.getDomainAxis.setRange(-1, 1)
    c.setBackgroundPaint(Color.white)
    c.getPlot.setBackgroundPaint(Color.white)
    c.getPlot.asInstanceOf[XYPlot].getDomainAxis.setAutoRange(true)
    c.getPlot.asInstanceOf[XYPlot].getRangeAxis.setAutoRange(true)
    c.getLegend.setVisible(true)
    c.getLegend.setItemFont(new Font(c.getLegend.getItemFont.getFontName, Font.PLAIN, 20))
  })

  private def toHumanReadable(e: LatexNamedTransformator) = e match {
    case IdentityTransformator => "Identity"
    case _ if e.getLatexHead("x").contains("rank") => "Rank Norm."
    case _ => "Linear Norm."
  }
  //	private val rawData = MetricValuesParser.getData.ignoreMetrics(Configuration.ignoredMetrics)
  (Plot.savedDirectory \ "MetaCorrelation").mkdir
  override def timedMain {
    for (sizeMetric <- List(NumberOfTokens, GZipRegularity, McCabesWithShortCircuit, DepthOfInheritance, NumberOfChildren);
         feature <- Configuration.goodFeatures) {
      println("Working on " + List(sizeMetric.shortName, feature))
      new MeanBugsToSize(sizeMetric, feature.shortName).apply(feature)
    }
  }
}
