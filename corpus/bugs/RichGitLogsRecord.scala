package corpus.bugs

import ast.ASTStringParser
import il.ac.technion.cs.ssdl.sablecc.GitLogsRecord
import mains.generators.tables.dataset.DatasetCorpusMagnitude
import parsers.git.GitAccessor

class RichGitLogsRecord(r: GitLogsRecord) extends GitAccessor {
	/**
   * the history of this record's file, from this revision to the first (timestamp descending)
   */
	lazy val history: Stream[GitLogsRecord] = {
		getRevisionsForFile(r.file)
			.toStream
			.map(getRevInformation(r.file, _).get)
	}

	def recordString: String = getVersion(r.file, r.revision).get

	lazy val ast = ASTStringParser.parse(r.file)

	def toBugRecord: BugRecord = new BugRecord(ast, r.file, r.revision)

	val timestamp = (DatasetCorpusMagnitude.dateFormat parse r.date).getMillis
}
object RichGitLogsRecord {
	implicit def rich(r: GitLogsRecord) = new RichGitLogsRecord(r)
}
