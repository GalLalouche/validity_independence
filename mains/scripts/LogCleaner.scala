package mains.scripts

import corpus.Corpus
import il.ac.technion.cs.ssdl.sablecc.GitLogsRecord
import mains.programs.Configuration
import parsers.git.{GitAccessor, LogsCsvParser}
import scala.collection.mutable.ListBuffer
import mains.programs.Debug

object LogCleaner extends GitAccessor with Debug {
	def clean(c: Corpus) {
		timed("Cleaning " + c) {
			val sortedLogFile = c.corpusDirectory / "logs_sorted.csv"
			val outputFile = sortedLogFile.parent.addFile("logs_cleaned.csv")
			val csvParser: LogsCsvParser = new LogsCsvParser()
			val list = ListBuffer[Seq[String]]()
			for (e <- csvParser.asSequence(sortedLogFile)) {
				val (file, revision) = {
					val temp = new GitLogsRecord(e.toArray, c.logFile.parent \ "Current")
					(temp.file, temp.revision)
				}
				if (getCommitType(file, revision) != GitAccessor.Deleted)
					list += e
			}
			assert(list.nonEmpty)
			csvParser.save(outputFile)(list)
		}
	}
	override def timedMain {
		Configuration.corpora foreach clean
	}
}
