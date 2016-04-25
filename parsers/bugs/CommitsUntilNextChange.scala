package parsers.bugs

import common.rich.RichT._
import common.rich.RichTuple._
import corpus.Corpus
import corpus.bugs.RichGitLogsRecord.rich
import il.ac.technion.cs.ssdl.sablecc.GitLogsRecord

// Double for compatibility with LifetimeUntilNextChange
object CommitsUntilNextChange extends BugParserGenerator[Double] {
	override def apply(c: Corpus) = new StabilityBugParser(c) {
		private val revisionsTimestamps = logs.sortBy(_.timestamp).zipWithIndex.map(e => e._1.revision -> e._2).toMap


		override protected def getStabilityBetween(x1: GitLogsRecord, x2: GitLogsRecord) =
			(x1, x2)
				.map(_.revision)
				.map(revisionsTimestamps)
				.mapTo(e => e._2 - e._1)
	}
	override def toString: String = "CommitsUntil NextChange"
}
