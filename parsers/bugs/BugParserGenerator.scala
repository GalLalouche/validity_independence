package parsers.bugs

import common.rich.path.RichPath.poorPath
import corpus._
import corpus.bugs._
import corpus.rich.RichCorpusData._
import mains.programs.Debug
import parsers.git.GitRecordsParser

/**
 * Generates a bug parser for the corpus.
 */
trait BugParserGenerator[T] extends (Corpus => BugParser[T]) with BugDataGenerator[T] with Debug {
	private[bugs] def getBugValues(c: Corpus): Seq[SimpleFileCommitValue[Option[T]]] = {
		val bp = this(c)
		val files = GitRecordsParser
			.getLogs(c.logFile)
			.map(FileCommit.apply)
		files map (e => new SimpleFileCommitValue(e, bp(e)))
	}

	private def getFilteredBugs(c: CorpusData, files: Map[String, Set[String]]): Seq[SimpleFileCommitValue[Option[T]]] =
		getBugValues(c.c).filter(e => files.get(e.file).exists(_.contains(e.revision)))
	// some files may not exist, since they did not compile and therefore only exist in the log but in the metric values data

	private def sorter(fc: FileCommit) = (fc.file, fc.revision)
	def joinBugs(c: CorpusData): CorpusDataWithBugs[T] = {
		val files = c.transpose.groupBy(_.file).map(e => e._1 -> e._2.map(_.revision).toSet)
		val bugsWithVersion = getFilteredBugs(c, files)
		zipBugs(c, bugsWithVersion)
	}

	override def joinBugs(cs: CorporaData): CorporaDataWithBugs[T] = timed(s"Joining bugs with " + this.toString) {
		new CorporaDataWithBugs(cs.corpora.par.map(joinBugs).seq)
	}

	protected def zipBugs(c: CorpusData, bugsWithVersion: Seq[SimpleFileCommitValue[Option[T]]]): CorpusDataWithBugs[T] =
		new CorpusDataWithBugs(c.c,
			c.transpose.sortBy(sorter) // it's important to sort before joining, to assure matching zips
				.zip(bugsWithVersion sortBy sorter)
				.collect { case (mv, bv) if bv.bugValue.isDefined => (mv, bv.map(_.get))}
				.map(e => MetricValuesForFileWithBugs.apply[T](e._1, e._2)) // join bug value with metric values
				.map(_.metricValuesWithBugs)
				.transpose
				.zip(c.metrics)
				.map(e => new MetricDataWithBugs(e._2, e._1)))
}
