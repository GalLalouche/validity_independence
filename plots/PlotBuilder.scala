package plots

import org.jfree.chart.JFreeChart
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYItemRenderer
import org.jfree.data.xy.{XYSeries, XYDataset, XYSeriesCollection}
import common.Builder

//Mutable, because I'm lazy
abstract class PlotBuilder[T <: PlotBuilder[T]](parameters: Map[String, Any]) extends Builder[T](parameters) {
	def getChart: JFreeChart
	def getRenderer: XYItemRenderer
	val f = parameters("chartModifier").asInstanceOf[JFreeChart => Unit]
	def build: Plot = {
		val chart = getChart
		val plot = chart.getPlot.asInstanceOf[XYPlot]
		plot.setRenderer(getRenderer)
		f(chart)
		new Plot(chart, asT("width"), asT("height"))
	}
	private def chartModifier(f: JFreeChart => Unit) = update(f)
	// will be executed after all the others
	def appendChartModifier(f: JFreeChart => Unit) = chartModifier(e => {this.f(e); f(e)})
	def title(s: String) = update(s)
	def nameX(s: String) = update(s)
	def nameY(s: String) = update(s)
	def height(h: Int) = update(h)
	def width(w: Int) = update(w)
	protected def data(data: XYDataset) = update(data)
	protected def toSeries(xs: Seq[(Double, Double)], name: String = "XYSeries"): XYSeriesCollection = {
		val series = new XYSeries(name)
		xs.foreach(e => series.add(e._1, e._2))
		new XYSeriesCollection(series)
	}
}

object PlotBuilder {
	// need to allow inheriting class to have default ctors
	val defaultMap = Map("width" -> 400, "height" -> 300, "title" -> "Chart Title",
		"chartModifier" -> ((e: JFreeChart) => ()), "nameX" -> "nameX", "nameY" -> "nameY",
		"data" -> new XYSeriesCollection())
}
