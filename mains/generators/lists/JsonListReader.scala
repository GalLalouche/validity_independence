package mains.generators.lists

import java.io.File

import common.rich.path.RichFile.richFile
import org.json4s._
import org.json4s.native.JsonMethods

// Reads the item description from a json file
abstract class JsonListReader[T](fileName: String) extends LatexDescriptionGenerator {
	private val projectsFile = new File(this.getClass.getResource(s"./$fileName.json").getFile)

	// Extracts the matching key of the item from the list
	protected def nameExtractor(t: T): String

	protected def getLabel(t: T, js: JValue): String
	protected def getContent(js: JValue): String

	require(projectsFile.exists, "Could not find projects file: " + projectsFile)
	private val summaries = JsonMethods parse (projectsFile readAll)
	implicit def jValueToString($: JValue): String = $.asInstanceOf[JString].values

	def apply(t: T) {
		try {
			val json = summaries \ nameExtractor(t)
			addItem(getLabel(t, json), getContent(json))
		} catch {
			case _: Exception => println("failed parsing json: " + nameExtractor(t))
		}
	}

	def apply(xs: Seq[T]) {
		xs foreach apply
	}
}