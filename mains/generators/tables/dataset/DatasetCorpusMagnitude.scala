package mains.generators.tables.dataset

import java.util.concurrent.TimeUnit

import common.CompositeDateFormat
import corpus._
import corpus.bugs.RichGitLogsRecord
import parsers.git.GitRecordsParser


object DatasetCorpusMagnitude extends DatasetMain {
	val dateFormat = CompositeDateFormat(
		"''yy-MM-dd",
		"yyyy-MM-dd HH:mm:ss",
		"yyyy-MM-dd",
		"EEE, dd MMM yyyy HH:mm:ss Z",
		"EEE MMM dd HH:mm:ss Z yyyy"
	)

	override def parseInfo(c: Corpus): Seq[Any] = {
		try {
			val logs = GitRecordsParser.getLogs(c.logFile.path)
			val byDates = logs sortBy (RichGitLogsRecord.rich(_).timestamp)
			val firstDate = dateFormat parse byDates.head.date
			val lastDate = dateFormat parse byDates.last.date
			val daysPassed = TimeUnit.MILLISECONDS.toDays(lastDate.getMillis - firstDate.getMillis)
			val numberOfContributors = logs.map(_.author).toSet.size
			Vector(c.fullName, dateFormat print firstDate, dateFormat print lastDate, daysPassed, numberOfContributors, byDates.last.revision.take(8))
		} catch {
			case e: Exception ⇒
				println(s"Exception of ${e.getClass.getName} type: ${e.toString}")
				throw e
		}
	}

	override def timedMain {
		//		Configuration.corpora.foreach(apply)
		DatasetSummarizer.timedMain
	}
}
