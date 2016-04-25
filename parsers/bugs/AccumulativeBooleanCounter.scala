package parsers.bugs

import corpus.{Corpus, FileCommit}


// this class counts the number of "true" values up to the last measured version
class AccumulativeBooleanCounter(bgp: BugParserGenerator[Boolean]) extends LastVersionBugParser[Boolean] {
	override protected def getBugsGenerator(c: Corpus): BugParser[Boolean] = bgp(c)
	override protected def getBugsValue(fc: FileCommit, lbp: LogsBugParser[Double], bg: BugParser[Boolean]) = {
		val allVersions = lbp.byFile(fc.file)
			.map(FileCommit.apply)
			.map(bg.apply)
		if (allVersions.forall(_.isDefined == false)) // if all values are None, then just return None
			None
		else // else count all trues
			Some(allVersions.count(e => e.isDefined && e.get))
	}
	override def toString: String = "Accumulative " + bgp
}