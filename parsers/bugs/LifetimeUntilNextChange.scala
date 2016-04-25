package parsers.bugs

import corpus.bugs.CorpusDataWithBugs
import corpus.{CorpusData, Corpus}
import corpus.bugs.RichGitLogsRecord._
import il.ac.technion.cs.ssdl.sablecc.GitLogsRecord

object LifetimeUntilNextChange extends BugParserGenerator[Double] {
	override def apply(c: Corpus) = new StabilityBugParser(c) {
		override protected def getStabilityBetween(x1: GitLogsRecord, x2: GitLogsRecord): Double =
			x2.timestamp - x1.timestamp
	}
	override def toString: String = "LifetimeUntil NextChange"
}
