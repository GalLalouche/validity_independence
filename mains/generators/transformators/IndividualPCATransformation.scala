package mains.generators.transformators

import corpus._
import corpus.bugs._
import corpus.rich.RichData._
import metrics.NumberOfTokens

object IndividualPCATransformation extends CorpusTransformator {

	override def apply(data: CorpusData): CorpusData = new PCATransformation(data.metricValuesOnly).apply(data)
	override def apply(data: CorporaData): CorporaData = super.apply(data).join(data.useMetrics(NumberOfTokens))

	override def applyWithBugs[T](data: CorporaDataWithBugs[T]): CorporaDataWithBugs[T] = super.applyWithBugs(data)
	override def apply[T](data: CorpusDataWithBugs[T]): CorpusDataWithBugs[T] = data.applyBugs(apply(data.asInstanceOf[CorpusData]))

}