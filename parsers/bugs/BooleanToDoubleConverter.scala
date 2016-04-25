package parsers.bugs

import corpus.{Corpus, FileCommit}


// this class counts the number of "true" values up to the last measured version
class BooleanToDoubleConverter(bgp: BugParserGenerator[Boolean]) extends BugParserGenerator[Double] {

	override def apply(c: Corpus) = {
		val parser = bgp(c)
		new BugParser[Double] {
			override def apply(v1: FileCommit): Option[Double] = parser(v1).map(if (_) 1.0 else 0.0)
		}
	}
	override def toString: String = "Boolean toDouble " + bgp
}
