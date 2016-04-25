package parsers.bugs

import common.rich.RichAll._

/**
* This class is use to aggregate all numerical bug data across entire repository history of a given file
*/
class AverageBugParser(obp: BugParserGenerator[Double]) extends AggregatorBugParser(obp, _.mean) {
	override def toString: String = "Average " + obp
}
