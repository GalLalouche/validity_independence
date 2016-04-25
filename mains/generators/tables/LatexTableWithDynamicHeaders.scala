package mains.generators.tables

//TODO this is horrible, fix!
class LatexTableWithDynamicHeaders extends LatexTableGenerator {
	def writeTable(headers: Option[Seq[String]], body1: Seq[Seq[Any]], body2: Seq[Seq[Any]]) {
		for (h <- headers) {
			appendTopRule
			writeLineToLatex(h map toLatexString)
		}
		appendMidRule
		body1.foreach(writeLineToLatex(_))
		appendMidRule
		body2.foreach(writeLineToLatex(_))
		appendBottomRule
	}
	def writeTable(headers: Seq[String], body: Seq[Seq[Any]]) { writeTable(Some(headers), body) }
	def writeTable(headers: Option[Seq[String]], body: Seq[Seq[Any]]) {
		for (h <- headers) {
			appendTopRule
			writeLineToLatex(h map toLatexString)
		}
		appendMidRule
		body.foreach(writeLineToLatex(_))
		appendBottomRule
	}
}
