package mains.generators.lists

import common.rich.path.Directory
import corpus.Corpus
import org.json4s._

// this is needed to ensure consistent corpora order
object ProjectsDescriptor extends JsonListReader[Corpus]("Projects") {
  override def nameExtractor(t: Corpus): String = t.fullName

  protected def getLabel(t: Corpus, js: JValue): String = js \ "name"
  protected def getContent(js: JValue): String = js \ "desc"
  override def apply(c: Corpus) {
    super.apply(c)
  } // stupid type erasure
  
  def timedMain(args: Array[String]) {
    apply(Corpus.corpora(Directory("D:/corpus")))
  }
}