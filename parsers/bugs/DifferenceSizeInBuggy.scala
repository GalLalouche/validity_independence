package parsers.bugs

import common.rich.RichT._
import common.rich.RichTuple._
import corpus.{FileCommit, Corpus}
import corpus.bugs.RichGitLogsRecord.rich
import il.ac.technion.cs.ssdl.sablecc.GitLogsRecord

// Double for compatibility with LifetimeUntilNextChange
object DifferenceSizeInBuggy extends BugParserGenerator[Double] {
	override def apply(c: Corpus) = new LogsBugParser[Double](c) {
		override def apply(fc: FileCommit) =
			Some(getNextVersion(fc)
				.filter(_.isBug)
				.map(_.diffs.size.toDouble)
				.getOrElse(0.0))
	}
	override def toString: String = "DifferenceSize InBuggy"
}
