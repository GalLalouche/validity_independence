package plots

import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.renderer.xy.{StandardXYBarPainter, XYBarRenderer}
import org.jfree.chart.{ChartFactory, JFreeChart}
import org.jfree.data.statistics.{SimpleHistogramBin, SimpleHistogramDataset}


class HistogramBuilder private(parameters: Map[String, Any]) extends PlotBuilder[HistogramBuilder](parameters) {
	def this() = this(PlotBuilder.defaultMap ++ Map("bars" -> 10, "nameY" -> "observations"))
	override def getRenderer: XYBarRenderer = {
		val $ = new XYBarRenderer
		$.setShadowVisible(false)
		$.setBarPainter(new StandardXYBarPainter)
		$
	}
	override def getChart: JFreeChart = ChartFactory
		.createXYBarChart(asT("title"),
			asT("nameX"),
			false,
			asT("nameY"),
			asT("data"),
			PlotOrientation.VERTICAL,
			true,
			false,
			false)
	private val _bars: Int = asT("bars")
	def bars(b: Int) = update(b)
	def data(xs: Seq[Double]) = {
		val binSize = xs.max / _bars
		val dataset = new SimpleHistogramDataset("Histogram")
		for (i <- 0 to _bars)
			dataset.addBin(new SimpleHistogramBin(i * binSize, (i + 1) * binSize, true, false))
		dataset.setAdjustForBinSize(false)
		xs.foreach { d => try dataset.addObservation(d) catch {case e: RuntimeException => println("No bin for " + d); throw e}}

		super.data(dataset)
	}
	override protected def clone(parameters: Map[String, Any]): HistogramBuilder = new HistogramBuilder(parameters)
}
