package corpus

import java.io.File

import common.rich.collections.RichTraversable.richTraversable
import common.rich.path.{RichFile, Directory}
import common.rich.path.RichFile.richFile
import common.rich.path.RichPath.poorPath
import mains.programs.Debug
import parsers.git.GitRecordsParser

/**
 * Represents a single Java project managed by GIT and residing in a single folder. This folder must include  a "log file" and
 * a GIT repository of the evolution of this project. The log file is expected to be the a GIT history file, which tells the story
 * of the evolution of each of the Java source files. Each file mentioned in the log must be be found in the repository. Similarly,
 * there should be at least one log entry for each of the source files. However, this later requirement is never checked.
 */
class Corpus(val corpusDirectory: Directory, val index: Char) {

	private val logFileName = "logs_cleaned.csv"
	lazy val logFile: RichFile = try {
		RichFile(corpusDirectory / logFileName)
	} catch {
		case e: Exception ⇒
			println(s"Cannot open file $logFileName in folder '$corpusDirectory'")
			null
	}

	override def toString = s"$index"
	lazy val (isValid, size): (Boolean, Int) = {
		try {
			val size = (GitRecordsParser getLogs logFile).size
			(true, size)
		} catch {
			case e: NullPointerException ⇒ (false, -1)
			case e: Exception ⇒
				println(s"Invalid logfile in folder $corpusDirectory [${e.getMessage}, caused by ${e.getCause.getMessage}]")
				(false, -1)
		}
	}

	val fullName = corpusDirectory.name
	override def hashCode(): Int = index.hashCode()
	override def equals(obj: scala.Any): Boolean = obj match {
		case other: Corpus => index == other.index
		case _ => false
	}
}

object Corpus extends Debug {
	private val corpusFile = new File("Corpus")
	if (corpusFile.exists == false) {
		println("Cannot find corpus folder: " + corpusFile.getAbsolutePath)
		println("... did your forget to mount it?")
		sys.exit(1)
	}
	private val corporaFolder = Directory(corpusFile)

	def corpora(d: Directory = Corpus.corporaFolder): Seq[Corpus] = timed("Retrieving Corpora") {
		val characters = ('A' to 'Z') ++ ('a' to 'z')
		require(d.dirs.nonEmpty, println("Could not find any corpora... did you forget to mount?"))
		d.dirs
			.sortBy(e => (-1 * (e \ "logs_sorted.csv").lines.size, e.name.toLowerCase))
			.zipWithIndex
			.map(e ⇒ (e._1, characters(e._2)))
			.tryMap(e ⇒ new Corpus(e._1, e._2))
			//      .filter(_.isValid) //TODO this check is taking too long (and sometimes fails because of uninteresting reasons) consider removing and putting it in the generator
			.toVector
	}
}
