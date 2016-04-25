package parsers.bugs

import common.rich.collections.RichSeq.richSeq
import common.rich.path.RichPath.poorPath
import corpus.bugs.RichGitLogsRecord.rich
import corpus.{Corpus, FileCommit}
import il.ac.technion.cs.ssdl.sablecc.GitLogsRecord
import parsers.git.GitRecordsParser

/**
 * this class helps in parsing the log file for deciding the bug value
 */
private[bugs] abstract class LogsBugParser[T](c: Corpus) extends BugParser[T] {
	val logs = GitRecordsParser.getLogs(c.logFile)
	lazy val lastRecord = logs
		.map(e => e.timestamp -> e)
		.sortBy(-_._1)
		.head
		._2

	lazy val byFile = logs
		.groupBy(FileCommit(_).file)
		.map(e => (e._1, e._2.sortBy(_.timestamp)))

	def getVersion(fc: FileCommit): GitLogsRecord = byFile(fc.file).find(fc.revision == _.revision).get

	def getNextVersion(fc: FileCommit): Option[GitLogsRecord] = {
		val revisions = byFile(fc.file)
		val nextIndex = revisions.findIndex(_.revision == fc.revision).get + 1
		if (nextIndex >= revisions.size) None else Some(revisions(nextIndex))
	}
	def getPrevVersion(fc: FileCommit): Option[GitLogsRecord] = {
		val revisions = byFile(fc.file)
		val prevIndex = revisions.findIndex(_.revision == fc.revision).get - 1
		if (prevIndex < 0) None else Some(revisions(prevIndex))
	}
}