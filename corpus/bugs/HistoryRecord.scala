package corpus.bugs

import corpus.bugs.RichGitLogsRecord._
import il.ac.technion.cs.ssdl.sablecc.GitLogsRecord

class HistoryRecord(val latestRecord: GitLogsRecord) {
  require(latestRecord != null)

  lazy val bugRecords: Stream[BugRecord] = latestRecord.history.map(_.toBugRecord)
}
