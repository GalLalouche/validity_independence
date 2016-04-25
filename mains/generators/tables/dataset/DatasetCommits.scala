package mains.generators.tables.dataset

import common.rich.collections.RichTraversableDouble._
import corpus.Corpus
import corpus.bugs.RichGitLogsRecord._
import corpus.rich.RichMetricData._
import mains.programs.{Configuration, MetricValuesParser}
import parsers.git.GitRecordsParser

object DatasetCommits extends DatasetMain {
	override def parseInfo(c: Corpus): Seq[Any] = {
		try {
			val logs = GitRecordsParser.getLogs(c.logFile.path)
			val byDates = logs sortBy (_.timestamp)
			val numberOfFileCommits = MetricValuesParser.readMetricValuesFromDisk(c).metricValues.head.length
			// this is not the size of the log file, as some entries are deletes
			val numberOfRepositoryCommits = logs.map(_.revision).toSet.size
			val filesInFirstVersion = logs.count(_.revision == byDates.head.revision)
			val filesAdded = logs.map(_.file).toSet.size - filesInFirstVersion
			val byFile = logs.groupBy(_.file).map(_._2.size)
			val medianNumberOfCommits = byFile.median.toInt
			val maxNumberOfCommits = byFile.max
			Vector(numberOfRepositoryCommits, numberOfFileCommits, filesInFirstVersion, logs.map(_.file).toSet.size,
				filesAdded, medianNumberOfCommits, maxNumberOfCommits)
		} catch {
			case e: Exception ⇒
				println(s"Exception of ${e.getClass.getName} type: ${e.toString}")
				throw e
		}
	}

	override def timedMain {
		Configuration.corpora.foreach(apply)
	}
}
