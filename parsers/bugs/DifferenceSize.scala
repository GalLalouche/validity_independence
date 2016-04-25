package parsers.bugs

import common.rich.RichT._
import corpus.{Corpus, FileCommit}


// Double for compatibility with LifetimeUntilNextChange
object DifferenceSize extends BugParserGenerator[Double] {
	override def apply(c: Corpus) = new LogsBugParser[Double](c) {
		override def apply(fc: FileCommit) = getNextVersion(fc).map(_.diffs.size.toDouble).getOrElse(0.0).opt
	}
}
