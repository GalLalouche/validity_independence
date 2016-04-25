package mains.programs

import java.io.File
import java.nio.file._

import common.rich.path.Directory
import rx.lang.scala.Observable

import scala.util.Try

/**
* Wrap NioWatcher with more meaningful messages
*/
object DirectoryWatcher {
  private def getEvent(p: Path, e: WatchEvent[_]) = {
    val file = p.resolve(e.context().asInstanceOf[Path]).toFile
    e.kind match {
      case StandardWatchEventKinds.ENTRY_DELETE |
           StandardWatchEventKinds.ENTRY_MODIFY if !file.exists =>
        FileDeleted(file)
      case StandardWatchEventKinds.ENTRY_CREATE =>
        if (file.isDirectory) DirectoryCreated(Directory(file))
        else FileCreated(file)
      case StandardWatchEventKinds.ENTRY_MODIFY if file.exists && !file.isDirectory =>
        FileModified(file)
    }
  }
  def apply(dir: Directory): Observable[DirectoryWatchEvent] = NioWatchObservable.apply(dir)
    .map(e => Try(getEvent(e._1, e._2)))
    .filter(_.isSuccess)
    .map(_.get)


  // msg types
  class DirectoryWatchEvent(val file: File) {
    override def equals(obj: scala.Any) = obj match {
      case event: DirectoryWatchEvent => file.equals(event.file)
      case _ => false
    }
    override def hashCode = file.hashCode()
  }
  case class DirectoryCreated(d: Directory) extends DirectoryWatchEvent(d.dir)
  class FileEvent(f: File) extends DirectoryWatchEvent(f)
  object FileEvent {
    def apply(f: File) = new FileEvent(f)
    def unapply(f: FileEvent) = Some(f.file)
  }
  case class FileDeleted(f: File) extends FileEvent(f)
  case class FileModified(f: File) extends FileEvent(f)
  case class FileCreated(f: File) extends FileEvent(f)
}
