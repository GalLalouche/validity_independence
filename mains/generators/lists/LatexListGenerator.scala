package mains.generators.lists

import common.rich.RichT._
import common.rich.path.RichFile.richFile
import mains.generators.LatexGenerator

trait LatexListGenerator extends LatexGenerator {
  def addItem(s: String) {
    file appendLine ("\\item " + s).log()
  }
  def addItems(xs: Seq[String]) {
    xs foreach addItem
  }
}
