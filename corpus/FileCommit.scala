package corpus

import corpus.bugs.BugRecord
import il.ac.technion.cs.ssdl.sablecc.GitLogsRecord


case class FileCommit private(file: String, revision: String) {
	def this(fc: FileCommit) = this(fc.file, fc.revision)
	def withoutRevision = new FileCommit(file, "N/A")
}

object FileCommit {
	def apply(record: GitLogsRecord): FileCommit = fromStrings(record.file.getCanonicalPath, record.revision)
	def apply(br: BugRecord):FileCommit = fromStrings(br.file.getCanonicalPath, br.revision)
	def fromStrings(file: String, revision: String):FileCommit =
		new FileCommit(file.substring(file indexOf "Corpus").replaceAll("\\\\", "/").replaceAll("Corpus/(Java/)?", "Corpus/"), revision)
}
