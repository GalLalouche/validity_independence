package plots

import org.jfree.chart.{JFreeChart, ChartFactory}
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.renderer.xy.{XYItemRenderer, XYSplineRenderer}
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}

import scala.collection.JavaConversions._

class ScatterPlotBuilder private(parameters: Map[String, Any]) extends PlotBuilder[ScatterPlotBuilder](parameters) {
	def this() = this(PlotBuilder.defaultMap)
	private val _data = parameters("data").asInstanceOf[XYSeriesCollection]
	override def getRenderer: XYItemRenderer = {
		val $ = new XYSplineRenderer
		$ setBaseShapesVisible false
		$
	}

	override protected def clone(parameters: Map[String, Any]): ScatterPlotBuilder = new ScatterPlotBuilder(parameters) {
		override def getRenderer = ScatterPlotBuilder.this.getRenderer
	}

	final override def getChart = {
		ChartFactory
			.createXYLineChart(asT("title"),
				asT("nameX"),
				asT("nameY"),
				_data,
				PlotOrientation.VERTICAL,
				true,
				false,
				false)
	}

	def addSeries(xs: Seq[(Double, Double)], name: String = "XYSeries") = {
		val newData = new XYSeriesCollection()
		for (s <- _data.getSeries.map(_.asInstanceOf[XYSeries]))
			newData addSeries s
		val series = new XYSeries(name)
		xs foreach (e => series.add(e._1, e._2))
		newData addSeries series
		data(newData)
	}
	def allSeries = _data.getSeries
	def addAll(xss: Seq[Seq[(Double, Double)]]): ScatterPlotBuilder = xss
		.foldLeft(this)((agg, c) => agg.addSeries(c))

	def resetSeries = data(new XYSeriesCollection())
}
