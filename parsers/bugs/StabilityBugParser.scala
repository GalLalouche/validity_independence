package parsers.bugs

import common.rich.RichT._
import corpus.{Corpus, FileCommit}
import il.ac.technion.cs.ssdl.sablecc.GitLogsRecord

private[bugs] abstract class StabilityBugParser(c: Corpus) extends LogsBugParser[Double](c) {
	// this method is used for approximating the last version of a file
	protected def getStabilityBetween(x1: GitLogsRecord, x2: GitLogsRecord): Double

	override def apply(fc: FileCommit): Option[Double] = {
		// who comments his code and found an actual use for currying? THIS GUY <-
		val partial = -1 * getStabilityBetween(getVersion(fc), _: GitLogsRecord) // -1 for unstability
		getNextVersion(fc)
			.map(partial)
			.getOrElse(2.0 * partial(lastRecord))
			.opt
	}
}
