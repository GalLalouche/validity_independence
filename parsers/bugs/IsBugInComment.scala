package parsers.bugs

import corpus.{Corpus, FileCommit}
import common.rich.RichT._

// Double for compatibility with LifetimeUntilNextChange
object IsBugInComment extends BugParserGenerator[Boolean] {
	override def apply(c: Corpus) = new LogsBugParser[Boolean](c) {
		override def apply(fc: FileCommit) = getNextVersion(fc).exists(_.isBug).opt
	}
	override def toString: String = "IsBugIn Comment"
}
