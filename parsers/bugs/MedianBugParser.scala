package parsers.bugs

import common.rich.collections.RichTraversableDouble._
import corpus.CorpusData
import corpus.rich.RichCorpusData._

/**
* This class is use to aggregate all numerical bug data across entire repository history of a given file
*/
class MedianBugParser(obp: BugParserGenerator[Double]) extends AggregatorBugParser(obp, _.median) {
	override def toString: String = "Median " + obp

	override protected def aggregateCorpusData(c: CorpusData): CorpusData = c.getMedianValuesPerFile
}
