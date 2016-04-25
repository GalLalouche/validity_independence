package plots

import java.awt.Graphics2D
import java.awt.geom.Rectangle2D
import java.io.{File, FileOutputStream}

import com.lowagie.text._
import com.lowagie.text.pdf._
import common.rich.path.Directory
import org.jfree.chart.JFreeChart
import org.sameersingh.scalaplot.jfreegraph.JFGraphPlotter

;

object Plot {
	val savedDirectory = Directory("./Generated/Plots")
}
class Plot private[plots](chart: JFreeChart, width: Int, height: Int) {
	val defaultFileName = chart.getTitle.getText.replaceAll(" ", "") + ".pdf"
	require(chart != null)

	import plots.Plot._

	def show() = {
		JFGraphPlotter gui chart
		this
	}
	def save() { save(savedDirectory \ defaultFileName) }
	def save(str: String) { save(new File(if (str.contains(".")) str else str + ".png")) }
	def save(file: File) {
		file.getParentFile.mkdirs()
		file.createNewFile
		saveAsPdf(file)
	}

	private def saveAsPdf(f: File) {
		val pageSize = new Rectangle(width, height)
		val document = new Document(pageSize, 50, 50, 50, 50)
		val writer = PdfWriter.getInstance(document, new FileOutputStream(f))
		document.open()
		val cb = writer.getDirectContent
		val tp = cb.createTemplate(width, height)
		val g2: Graphics2D = tp.createGraphics(width, height, new DefaultFontMapper())
		chart.draw(g2, new Rectangle2D.Double(0, 0, width, height))
		g2.dispose()
		cb.addTemplate(tp, 0, 0)
		document.close()
		writer.flush()
		writer.close()
	}

	def saveAsPdf() {
		val pageSize = new Rectangle(width, height)
		val document = new Document(pageSize, 50, 50, 50, 50)
		val writer = PdfWriter.getInstance(document, new FileOutputStream("C:/test.pdf"))
		document.open()
		val cb = writer.getDirectContent
		val tp = cb.createTemplate(width, height)
		val g2: Graphics2D = tp.createGraphics(width, height, new DefaultFontMapper())
		chart.draw(g2, new Rectangle2D.Double(0, 0, width, height))
		g2.dispose()
		cb.addTemplate(tp, 0, 0)
		document.close()
	}
}
