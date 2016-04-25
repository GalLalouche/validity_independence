package parsers.bugs

import common.rich.RichAll._
import corpus._

/**
* This class is use to aggregate all numerical bug data across entire repository history of a given file
*/
private[bugs] class AggregatorBugParser(originalParserGenerator: BugParserGenerator[Double], aggregator: Seq[Double] => Double) extends LastVersionBugParser[Double] {
	override protected def getBugsGenerator(c: Corpus): BugParser[Double] = originalParserGenerator(c)
	override protected def getBugsValue(fc: FileCommit, lbp: LogsBugParser[Double], parser: BugParser[Double]) =
		lbp
			.byFile(fc.file) // selects the file
			.map(FileCommit.apply) // converts to a file commit
			.map(parser.apply) // gets composed bug value
			.mapTo(e => e.filter(_.isDefined).map(_.get))
			.mapTo(e => if (e.isEmpty) None else Some(e))
			.map(aggregator)
	override def toString: String = "Average " + originalParserGenerator
}
