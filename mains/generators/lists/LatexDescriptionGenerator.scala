package mains.generators.lists

import common.rich.RichT._
import common.rich.path.RichFile.richFile
import mains.generators.LatexGenerator

trait LatexDescriptionGenerator extends LatexGenerator {
  def addItem(label: String, desc: String) {
    file appendLine s"\\item[$label] ${desc.mapIf(false == _.matches("\\s|.*?\\.\\$*$")).to(_ + ".")}".log()
  }
}
