package corpus

import common.rich.collections.RichTraversable._
import corpus.rich.RichCorpusData._

class CorporaData(val corpora: Seq[CorpusData]) {
  require(corpora nonEmpty, "data is empty")
  require(corpora.hasSameValues(_.metrics), "found different metrics in different corpora\n" + corpora.map(_.metrics).mkString("\n"))
}
