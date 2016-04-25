package parsers.bugs

import common.rich.RichT._
import corpus.bugs.{CorporaDataWithBugs, CorpusDataWithBugs}
import corpus.rich.RichCorpusData
import corpus.rich.RichCorpusData._
import corpus.{CorporaData, CorpusData}
import mains.programs.MetricValuesParser

object BugDataGenerator {

	def filterOptional[T](data: CorporaDataWithBugs[Option[T]]): CorporaDataWithBugs[T] = {
		data.corpora
			.map(cd => cd.transpose
			.collect {
			case e if e.bugValue.isDefined => e.withBugValue(e.bugValue.get)
		}.mapTo(RichCorpusData.buildFromTransposedData(cd.c, _)))
			.mapTo(new CorporaDataWithBugs(_))
	}
}

trait BugDataGenerator[T] {
	def getData: CorporaDataWithBugs[T] = joinBugs(MetricValuesParser.getData)
	def joinBugs(cs: CorporaData): CorporaDataWithBugs[T]
	def joinBugs(cs: CorpusData): CorpusDataWithBugs[T]
	override def toString = this.simpleName
}
