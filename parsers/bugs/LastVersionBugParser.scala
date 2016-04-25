package parsers.bugs

import corpus._
import corpus.bugs._
import corpus.rich.RichCorpusData._

/**
* This class is used to aggregate all across file versions, and uses the average metric value
*/
private[bugs] trait LastVersionBugParser[T] extends BugParserGenerator[Double] {
	protected def getBugsGenerator(c: Corpus): BugParser[T]
	protected def getBugsValue(fc: FileCommit, lbp: LogsBugParser[Double], parser: BugParser[T]): Option[Double]

	override def apply(c: Corpus) = new LogsBugParser[Double](c) {
		val parser = getBugsGenerator(c)
		override def apply(fc: FileCommit): Option[Double] =
			if (byFile(fc.file).last.revision != fc.revision)
				None
			else
				getBugsValue(fc, this, parser)
	}
	private def removeRevision(e: SimpleFileCommitValue[Option[Double]]) =
		new SimpleFileCommitValue[Option[Double]](e.withoutRevision, e.bugValue)

	override protected def zipBugs(c: CorpusData, bugsWithVersion: Seq[SimpleFileCommitValue[Option[Double]]]) =
		super.zipBugs(aggregateCorpusData(c),
			bugsWithVersion
				.groupBy(_.file) // groups by file, and finds a defined representative or takes the head if none could be found
				.map(e => e._2.find(_.bugValue.isDefined).getOrElse(e._2.head)) //
				.map(removeRevision)
				.toVector
		)
	protected def aggregateCorpusData(c: CorpusData): CorpusData = c.getAverageValuesPerFile
}
