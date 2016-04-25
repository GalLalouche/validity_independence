package parsers.git

import java.io.File

import ast.ASTStringParser
import common.rich.path.RichFile.richFile
import corpus.bugs.BugRecord
import corpus.bugs.RichGitLogsRecord.rich
import il.ac.technion.cs.ssdl.sablecc.GitLogsRecord
import mains.programs.Debug

object GitRecordsParser extends Debug with GitAccessor {
  protected def getPreviousVersion(log: GitLogsRecord): Option[String] = {
    super.getPreviousVersion(log.file, log.revision)
  }

  def parse(f: File): Iterator[BugRecord] = parse(f getAbsolutePath)

  def parse(logFilePath: String): Iterator[BugRecord] =
    getLogs(logFilePath)
      .iterator
      .filter(log => hasVersion(log.file, log.revision))
      .map(log => new BugRecord(ASTStringParser.parse(getVersion(log.file, log.revision).get), log.file, log.revision))

  private val csvReader = new LogsCsvParser()
  def getLatestLogs(logFilePath: String): Seq[GitLogsRecord] = {
    val latestLogs = getLogs(logFilePath)
      .groupBy(_.file)
      .map(_
        ._2
        .sortBy(_.date)
        .reverse(0))
      .toVector
    println(s"There are ${latestLogs.size} latest files in ${new File(logFilePath).parent}")
    latestLogs
  }

  def getLogs(logFile: File): Seq[GitLogsRecord] = {
    csvReader
      .asSequence(logFile.path)
      .zipWithIndex
      .map(e ⇒ try {
        new GitLogsRecord(e._1.toArray, logFile.parent \ "Current")
      } catch {
        case x: Exception ⇒ throw new Exception(s"caughed exception at index ${e._2}", x)
      })
      .filterNot(_.revision == "RevisionId")
  }

  /**
   * grouped by file name, ordered by date
   */
  def getLogsByFileByDate(logFile: File): Map[String, Seq[GitLogsRecord]] = {
    getLogs(logFile)
      .groupBy(_.file.toString)
      .map(e => e._1 -> e._2.sortBy(_.timestamp))
      .toMap
  }

  def getLogs(logFilePath: String): Seq[GitLogsRecord] = getLogs(new File(logFilePath))
}