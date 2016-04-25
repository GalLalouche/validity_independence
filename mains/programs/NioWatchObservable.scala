package mains.programs

import java.nio.file._

import common.rich.path.Directory
import rx.lang.scala.schedulers._

import scala.collection.JavaConverters._
import scala.collection.mutable

/**
* Transform java's nio api for watching directories into an observable
*/
object NioWatchObservable {
  private implicit def dirToPath(d: Directory): Path = Paths.get(d.path)
  def apply(dir: Directory) = rx.lang.scala.Observable.apply[(Path, WatchEvent[_])](s => {
    val keys = mutable.HashMap[WatchKey, Path]()

    val watchService = dir.getFileSystem.newWatchService

    /**  Recursively register directories */
    def registerAll(dir: Directory) {
      keys(dir.register(watchService,
        StandardWatchEventKinds.ENTRY_CREATE,
        StandardWatchEventKinds.ENTRY_MODIFY,
        StandardWatchEventKinds.ENTRY_DELETE)) = dir
      dir.dirs foreach registerAll
    }
    registerAll(dir)

    while (s.isUnsubscribed == false) {
      val key = watchService.take()
      val publishingPath = keys(key)
      val events = key.pollEvents().asScala
      events
        .filter(_.kind == StandardWatchEventKinds.ENTRY_CREATE)
        .map(e => publishingPath.resolve(e.context.asInstanceOf[Path]))
        .filter(Files.isDirectory(_, LinkOption.NOFOLLOW_LINKS))
        .foreach(c => registerAll(Directory(c.toAbsolutePath.toFile)))
      if (key.reset() == false)
        keys.remove(key)
      events.foreach(s.onNext(publishingPath, _))
    }
  }).subscribeOn(NewThreadScheduler()) // mandatory as the watcher blocks
}
