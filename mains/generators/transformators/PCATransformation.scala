package mains.generators.transformators

import Jama.Matrix
import common.rich.RichAll._
import corpus._
import corpus.bugs._
import corpus.rich.RichData._
import metrics.NumberOfTokens
import stats.RichMatrix._
import stats.{PCA, PCAMetric}

class PCATransformation(pcaMatrix: Matrix) extends CorpusTransformator {
	require(pcaMatrix != null)
	def this(data: Seq[Seq[Double]]) = this(PCA.apply(data))
	def this(data: CorpusData) = this(PCA.apply(data))
	private def getPCAMetricValuesForFile(data: Seq[Double]) = pcaMatrix timesCol data

	private def calcRawData_(data: Seq[Seq[MetricValue]]): Seq[Seq[MetricValue]] = {
		data
			.transpose
			.map(e => e zip (e map (_.metricValue) mapTo getPCAMetricValuesForFile))
			.map(_ map (e => e._1.withMetricValue(e._2)))
			.transpose
	}
	private def calcRawData(data: Seq[Seq[MetricValue]]): Seq[Seq[MetricValue]] =
		calcRawData_(data) // If the data per metric value was negative, take its negative value
			.map(_ mapIf (_.map(_.metricValue).sum < 0) to (_ map (_.mapMetricValue(-_))))
	override def apply(data: CorpusData): CorpusData = {
		data.metricValues
			.map(_.metricValues)
			.mapTo(calcRawData)
			.zipWithIndex
			.flatZip(data.metricValues)
			.map { case (newValues, index, originalData) => new MetricData(
			new PCAMetric(pcaMatrix.rows(index) zip data.metrics, index),
			originalData.metricValues.zip(newValues).map(e => e._1.withMetricValue(e._2.metricValue)))
		}
			.mapTo(new CorpusData(data.c, _))
	}


	override def apply(data: CorporaData): CorporaData = super.apply(data).join(data.useMetrics(NumberOfTokens))

	override def applyWithBugs[T](data: CorporaDataWithBugs[T]): CorporaDataWithBugs[T] = super.applyWithBugs(data)
	override def apply[T](data: CorpusDataWithBugs[T]): CorpusDataWithBugs[T] = data.applyBugs(apply(data.asInstanceOf[CorpusData]))

}
