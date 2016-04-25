package common.os

import java.io.File

import common.rich.RichAll._

import scala.sys.process.Process

object RichWindows extends RichOs {
  override def getAssociation(file: File): String = {
    val extension = file.extension
    val assoc = Process("cmd /c assoc ." + extension).!!.split("=")(1)
    val macroedPath = Process("cmd /c ftype " + assoc).!!.split("=")(1).trim.captureWith( """.*?"?([^"]+)"?.*""".r)
    val actualPath: String = macroedPath.substring(0, macroedPath.indexOf("%1").mapIf(_ < 0).to(e => macroedPath.length + 1) - 1)
    Process("cmd /c dir \"" + actualPath + "\"").!!
      .split("\r?\n")
      .map(_.trim)
      .find(_.startsWith("Directory of"))
      .get
      .mapTo(_.captureWith("Directory of (.*)".r) + "\\" + macroedPath.dropAfterLast('\\').captureWith("\\.[A-z]{3}".r))
  }
  override def getRunningProcesses: Seq[ProcessInfo] = {
    val cmdList = Process("WMIC PROCESS get Caption,Commandline,Processid").!!
      .split("\r?\n")
    val head = cmdList.head.toLowerCase
    val secondIndex = head.indexOf("commandline")
    val thirdIndex = head.indexOf("processid")
    cmdList
      .tail
      .filter(_.length > 1)
      .toStream
      .map(e => {
      List(e.substring(0, secondIndex - 2),
        e.substring(secondIndex, thirdIndex - 2),
        e.substring(thirdIndex)
      ).map(_.trim)
    }).map(e => ProcessInfo(e(0), e(1), e(2).toInt))
      .toVector
  }
  override def kill(pid: Int) {
    Runtime.getRuntime.exec("taskkill /F /PID " + pid)
  }
}
