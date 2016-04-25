package parsers.bugs

import corpus.{Corpus, FileCommit}


// this class counts the number of "true" values up to the last measured version
class AccumulativeDoubleCounter(bgp: BugParserGenerator[Double]) extends LastVersionBugParser[Double] {
	override protected def getBugsGenerator(c: Corpus): BugParser[Double] = bgp(c)
	override protected def getBugsValue(fc: FileCommit, lbp: LogsBugParser[Double], bg: BugParser[Double]) = {
		val allVersions = lbp.byFile(fc.file)
			.map(FileCommit.apply)
			.map(bg.apply)
		if (allVersions.forall(_.isDefined == false)) // if all values are None, then just return None
			None
		else // else count all trues
			Some(allVersions.filter(e => e.isDefined).map(_.get).sum)
	}
	override def toString: String = "Accumulative " + bgp
}
