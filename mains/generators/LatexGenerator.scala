package mains.generators


import common.rich.path.Directory

class LatexGenerator(_tableName: Option[String])
	extends FileGenerator(_tableName, "tex") {

	override def directory: Directory = Directory("./Manuscripts/Tables")
	def this() = this(None)
	def this(name: String) = this(Some(name))

	sealed class Alignment(val str: String)

	object right extends Alignment("r")
	object center extends Alignment("c")
	object left extends Alignment("l")

	implicit protected class LatexString(str: String) {
		def emph: String = wrapInMacro("emph")
		def textbf: String = wrapInMacro("textbf")
		def wrapInMultiColumn(alignment: String, number: Int): String = s"\\multicolumn{$number}{$alignment}{$str}"
		def align(a: Alignment) = {
			wrapInMultiColumn(a.str, 1)
		}
		def wrapInMacro(macroName: String) = s"\\$macroName{$str}"
	}
}
