package mains.generators.transformators

import corpus._
import corpus.bugs.CorporaDataWithBugs

trait CorporaTransformator {
	def apply(data: CorporaData): CorporaData
	def applyWithBugs[T](data: CorporaDataWithBugs[T]): CorporaDataWithBugs[T] =
		data.applyBugs(CorporaTransformator.this.apply(data.asInstanceOf[CorporaData]))
}