package mains.generators.transformators

import corpus._
import corpus.bugs._
import corpus.rich.RichData._

trait CorpusTransformator extends CorporaTransformator {
	def apply(data: CorpusData): CorpusData
	def apply[T](data: CorpusDataWithBugs[T]): CorpusDataWithBugs[T] = data applyBugs apply(data.asInstanceOf[CorpusData])
	override def apply(data: CorporaData): CorporaData = data.map(this.apply)
}
