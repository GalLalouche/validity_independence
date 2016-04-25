package plots

import java.awt.Color

import common.TupleFunction._
import common.rich.RichAll._
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.data.function.LineFunction2D
import org.jfree.data.general.DatasetUtilities
import stats.Pearson
import stats.regression.LinearRegression

import scala.collection.mutable.ListBuffer

class RegressionBuilderDecorator private(pb: ScatterPlotBuilder, data: Seq[(Double, Double)]) {
	val seriesIndex = pb.allSeries.size
	val regressionName = "Regression"
	def this(pb: ScatterPlotBuilder) = this(pb, ListBuffer())
	// more efficient ++ operations
	def addSeries(xs: Seq[(Double, Double)], name: String = "XYSeries") =
		new RegressionBuilderDecorator(pb.addSeries(xs, name), data ++ xs)
	def buildWithRegression = {
		val renderer = new XYLineAndShapeRenderer(true, false)
		renderer.setSeriesPaint(0, Color.black)
		pb.appendChartModifier(c => {
			c.getXYPlot.setDataset(seriesIndex, DatasetUtilities.sampleFunction2D(
				LinearRegression.linearRegression(data).mapTo(r => new LineFunction2D(r.intercept, r.slope)),
				data.map(_._1).min - 0.1 * Math.abs(data.map(_._1).min),
				data.map(_._1).max + 0.1 * Math.abs(data.map(_._1).max),
				2,
				s"$regressionName (R² = ${Pearson.apply(data.unzip).sq.withPrecision(2)})"))
			c.getXYPlot.setRenderer(seriesIndex, renderer)
		}).build
	}
}
