package mains.generators.tables.dataset

import java.text.SimpleDateFormat

import corpus.Corpus
import il.ac.technion.cs.ssdl.sablecc.GitLogsRecord
import mains.generators.tables.dataset.DatasetCommits._
import mains.programs.Configuration
import parsers.git.{GitAccessor, GitRecordsParser}

import scala.collection.mutable.ListBuffer

object DatasetSizeCounter extends DatasetMain with GitAccessor {

	override protected val formatTableItem: PartialFunction[Any, Any] = {
		case x: Double => x.toInt
	}
	private val df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
	private def getChanges(implicit logs: Seq[GitLogsRecord]): Seq[Int] =
		getTriplet(logs groupBy (_ file) map (e => (e._1, e._2.sortBy(e => df.parse(e.date)).drop(1))))
	private def getNewFiles(implicit logs: Seq[GitLogsRecord]): Seq[Int] =
		getTriplet(logs groupBy (_ file) map (e => (e._1, e._2.sortBy(e => df.parse(e.date)).take(1))))

	override def parseInfo(c: Corpus): Seq[Any] = {
		implicit val logs: Seq[GitLogsRecord] = GitRecordsParser.getLogs(c.logFile.path)
		ListBuffer(logs.size) ++ getChanges ++ getNewFiles
	}

	private def getTriplet(byFile: Map[java.io.File, Seq[GitLogsRecord]])
												(implicit logs: Seq[GitLogsRecord]): Seq[Int] = {
		def percentageOfSizeFromTotal(e: Int)(implicit logs: Seq[GitLogsRecord]): Int =
			((100.0 * e) / logs.size).round.toInt

		val total = byFile.map(_._2.size).sum
		val buggy = byFile.map(_._2.count(_.isBug)).sum
		val other = byFile.map(_._2.filterNot(_.isBug).size).sum
		ListBuffer(total, percentageOfSizeFromTotal(total)) ++
			ListBuffer(buggy, percentageOfSizeFromTotal(buggy)(byFile flatMap (_._2) toSeq)) ++
			ListBuffer(other, percentageOfSizeFromTotal(other)(byFile flatMap (_._2) toSeq))
	}

	override protected def timedMain {
		DatasetSummarizer(this)
	}
}
