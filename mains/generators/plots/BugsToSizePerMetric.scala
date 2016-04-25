package mains.generators.plots

import java.awt.Color

import common.rich.RichT._
import common.rich.primitives.RichDouble._
import corpus.rich.RichData._
import mains.generators.metricValues.FeatureParser
import mains.programs._
import metrics._
import metrics.post.DepthOfInheritance
import org.jfree.chart.plot.XYPlot
import org.jfree.ui.RectangleEdge
import org.jfree.util.ShapeUtilities
import plots._
import stats._


object BugsToSizePerMetric extends Debug {
	val correlation: Correlation = KendallTau
	val metricUnderTest = DepthOfInheritance
	val features = Configuration.goodFeatures
	override def timedMain {
		for (f <- features) {
			val data = FeatureParser.load(f)
			val (correlationWithSize, correlationWithFeature) = data.corpora
				.map(e => (e(metricUnderTest).metricValuesOnly, e(NumberOfTokens).metricValuesOnly, e.bugs))
				.map(e => KendallTau(e._1, e._2) -> KendallTau(e._1, e._3))
				.sortBy(e => Math.abs(e._2))
				.filterNot(e => Math.abs(e._2) < 0.12)
				.unzip
			println("R^2 is: " + Pearson(correlationWithSize, correlationWithFeature).sq)
			new ScatterPlotBuilder() {
				override def getRenderer = {
					val $ = new org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
					$.setBaseLinesVisible(false)
					$.setUseFillPaint(false)
					$.setDrawOutlines(false)
					$.setUseOutlinePaint(false)
					$.setSeriesShape(0, ShapeUtilities.createDownTriangle(6))
					$
				}
			}.title(s"Correlation of $metricUnderTest to $NumberOfTokens and ${f.shortName}")
				.nameX("Dependence")
				.nameY(s"Validity (${f.shortName})")
				.width(600)
				.appendChartModifier(c => {
				c.getLegend.setPosition(RectangleEdge.RIGHT)
				c.setBackgroundPaint(Color.white)
				c.getPlot.setBackgroundPaint(Color.white)
				c.getPlot.asInstanceOf[XYPlot].getDomainAxis.setAutoRange(true)
				c.getPlot.asInstanceOf[XYPlot].getRangeAxis.setAutoRange(true)
				c.getLegend.setVisible(true)
			})
				.mapTo(new RegressionBuilderDecorator(_))
				.addSeries(correlationWithSize zip correlationWithFeature, metricUnderTest + " values")
				.buildWithRegression
				.save((Plot.savedDirectory / "dit" / "filtered") \ (f.shortName.replaceAll(" ", "") + ".pdf"))
		}
	}
}
