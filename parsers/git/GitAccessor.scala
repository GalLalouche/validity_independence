package parsers.git

import java.io.File
import java.nio.file.{ Path, Paths }

import common.rich.collections.RichSeq._
import common.rich.path.Directory
import common.rich.path.RichFile.richFile
import common.rich.path.RichPath.poorPath
import common.rich.primitives.RichString._
import il.ac.technion.cs.ssdl.sablecc.GitLogsRecord
import org.eclipse.jgit.errors.MissingObjectException
import org.eclipse.jgit.lib.Repository
import org.eclipse.jgit.revwalk.RevWalk
import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import org.eclipse.jgit.treewalk.TreeWalk
import parsers.git.GitAccessor._

import scala.annotation.tailrec
import scala.sys.process.Process

trait GitAccessor {

  private def getRepository(file: java.io.File): org.eclipse.jgit.lib.Repository = {
    @tailrec
    def getGitDirectory(file: File): Directory = {
      if (file.isDirectory && Directory(file).dirs.exists(_.name == ".git"))
        Directory(file) / ".git" /
      else
        getGitDirectory(file getParentFile)
    }
    new FileRepositoryBuilder()
      .setGitDir(getGitDirectory(file))
      .readEnvironment
      .setup
      .build
  }

  protected def hasVersion(file: File, rev: String): Boolean = {
    try {
      getCommitType(file, rev) != Deleted
    } catch {
      case _: NoSuchElementException => false
      case e: java.lang.RuntimeException if e.getMessage == "Nonzero exit value: 128" => false
    }
  }
  protected def getNextRevision(file: File, rev: String): Option[String] = {
    val revisions = getRevisionsForFile(file)
    val index = revisions.findIndex(_ == rev).get
    if (index > 0)
      Some(revisions(index - 1))
    else
      None
  }

  protected def getPreviousVersion(file: File, rev: String): Option[String] = {
    val repository = getRepository(file)
    val reader = repository.newObjectReader()
    try {
      val id = repository.resolve(rev)
      val walk = new RevWalk(reader)
      val commit = walk.parseCommit(id)
      if (commit.getParentCount > 0) {
        val parent = walk.parseCommit(commit.getParent(0).getId)
        require(parent != null)
        val tree = parent.getTree
        require(tree != null)
        val relativePath = getRelativePath(file, repository)
        val treewalk = TreeWalk.forPath(reader, relativePath.toFile.toString.replaceAll("\\\\", "/"), tree)
        if (treewalk != null)
          Some(new String(reader.open(treewalk.getObjectId(0)).getBytes, "utf-8"))
        else
          None
      } else
        None
    } catch {
      case e: MissingObjectException ⇒ None
      case e: NullPointerException   ⇒ None
    } finally {
      repository.close()
    }
  }

  private def getRelativePath(file: File, repository: Repository): Path =
    Paths.get(repository.getDirectory.getParent).relativize(Paths.get(file.getAbsolutePath))

  protected def getRevInformation(file: File, rev: String): Option[GitLogsRecord] = {
    val repository = getRepository(file)
    val reader = repository.newObjectReader()
    try {
      val id = repository.resolve(rev)
      val walk = new RevWalk(reader)
      val commit = walk.parseCommit(id)
      val author = commit.getAuthorIdent
      val msg = commit.getFullMessage
      Option(new GitLogsRecord(file,
        commit.getId.getName,
        author.getWhen.toString,
        author.getName,
        msg.trim.withoutTrailingQuotes,
        null))
    } catch {
      case e: MissingObjectException ⇒ None
      case e: NullPointerException   ⇒ None
    } finally {
      repository.close()
    }
  }
  protected def getVersion(file: File, rev: String): Option[String] = {
    val repository = getRepository(file)
    val reader = repository.newObjectReader
    try {
      val id = repository.resolve(rev)
      val walk = new RevWalk(reader)
      val commit = walk parseCommit id
      val tree = commit.getTree
      val relativePath = getRelativePath(file, repository)
      val treewalk = TreeWalk.forPath(reader, relativePath.toFile.toString.replaceAll("\\\\", "/"), tree)
      if (treewalk != null)
        new Some(new String(reader.open(treewalk.getObjectId(0)).getBytes, "utf-8"))
      else
        None
    } catch {
      case e: MissingObjectException ⇒ None
      case e: NullPointerException ⇒
        println(s"Null pointer error when retrieving revision $rev of ${file.toString}")
        None // the revision/file is not missing, but simply doesn't exist yet
    } finally {
      repository.close()
    }
  }

  /**
   * Returns all revisions of the file, in DESCENDING order (i.e., from latest to earliest)
   */
  protected def getRevisionsForFile(file: File): Seq[String] = {
    val repository = getRepository(file)
    try {
      val relativePath = getRelativePath(file, repository)
      val p = Process("""git log --pretty=format:"%H" -- """ + relativePath, repository.getDirectory.parent)
      val logResult: String = p.!!
      logResult
        .split("\n")
        .map(_.trim.withoutTrailingQuotes)
    } finally {
      repository.close()
    }
  }

  protected def getCommitType(file: File, rev: String): CommitType = {
    val repository = getRepository(file)
    try {
      Process(s"git show $rev --name-status --oneline", repository.getDirectory.parent).!!
        .split("\n")
        .drop(1)
        .filterNot(_.trim == "")
        .map(_ split "\\s")
        .find(_(1) == getRelativePath(file, repository).toString.replaceAll("\\\\", "/").trim)
        .get
        .apply(0).toList match {
          case 'A' :: str => Added
          case 'M' :: str => Modified
          case 'D' :: Nil => Deleted
          case e          => throw new AssertionError("Unsupported commit type " + e)
        }
    } finally {
      repository.close()
    }
  }
}

object GitAccessor {

  import common.rich.RichT._

  sealed class CommitType {
    override def toString: String = this.simpleName
  }
  object Added extends CommitType
  object Modified extends CommitType
  object Deleted extends CommitType
}
