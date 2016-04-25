package mains.generators.transformators

import corpus.{CorpusData, CorporaData}
import corpus.bugs.{CorpusDataWithBugs, CorporaDataWithBugs}

object IdentityTransformator extends SimpleTransformator {
	override def apply(d: Double) = d
	def getLatexHead(variableName: String) = variableName
	override def apply(data: CorpusData): CorpusData = data
	override def apply(data: CorporaData): CorporaData = data
	override def apply[T](data: CorpusDataWithBugs[T]): CorpusDataWithBugs[T] = data
	override def applyWithBugs[T](data: CorporaDataWithBugs[T]): CorporaDataWithBugs[T] = data
}
