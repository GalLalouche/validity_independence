package mains.generators.tables.dataset

import corpus.Corpus
import mains.generators.tables.LatexTableGenerator
import mains.programs.{Configuration, Debug}

abstract class DatasetMain extends LatexTableGenerator with Debug {
  protected def parseInfo(c: Corpus): Seq[Any]

  private def appendCorpusDataToTable(c: Corpus) {
    timed(s"Calculating $c properties into ${file.name }") {
      val line = c.index :: parseInfo(c).toList
      println(line)
      writeLineToLatex(line)
    }
  }

  def apply(c: Corpus) {
    appendCorpusDataToTable(c)
  }

  def apply() {
    file.createNewFile()
    file.clear
    Configuration.corpora.foreach(apply)
  }

	override protected def timedMain {
		Configuration.corpora.foreach(apply)
	}
}
