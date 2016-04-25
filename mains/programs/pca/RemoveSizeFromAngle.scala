package mains.programs.pca

import common.rich.RichAll._
import corpus.rich.RichData._
import corpus.{CorporaData, CorpusData}
import mains.generators.tables.LatexTableGenerator
import mains.programs.{Configuration, Debug, MetricValuesParser}
import stats._

object RemoveSizeFromAngle extends LatexTableGenerator with Debug {

	// mean -> std
	private def calcMeanAngle(data: Seq[Seq[Vector[Double]]]): (Double, Double) =
		data
			.map(_.take(300))
			.getUniquePairs // unique corpora pairs
			.map(calcMeanAngleRaw)
			.toSeq
			.unzip
			.map(_.mean)

	private def calcMeanAngleRaw(e: (Seq[Vector[Double]], Seq[Vector[Double]])): (Double, Double) = {
		val cosines = e.zip.map { case (x, y) => x.cosineSimilarityTo(y)}
		cosines.mapTo(e => e.mean -> e.standardDeviation)
	}

	private def getVectorsWithoutSize(data: CorpusData): Seq[Vector[Double]] = {
		val sizeVector: Vector[Double] = data
			.metricValuesOnly
			.mapTo(PCA.getVectorsWithEigenValues(_).map(_._1).head.toVector)
		data
			.getVectors
			.map(_ antiVector sizeVector)
	}
	private def getVectorsWithoutSize(data: CorporaData): Seq[Seq[Vector[Double]]] = data.corpora.map(getVectorsWithoutSize)

	case class Angles(meanAngle: Double, stdMeanAngle: Double, meanAngleWithoutSize: Double, meanAngleWithoutSizeStd: Double)

	def getAngles(c1: CorpusData, c2: CorpusData): Angles = {
		def aux(c: CorpusData) =
			c.getVectors -> getVectorsWithoutSize(c)
		val ((v1, vwos1), (v2, vwos2)) = c1 -> c2 map aux
		val r = calcMeanAngleRaw(v1, v2)
		val wos = calcMeanAngleRaw(vwos1, vwos2)
		Angles(r._1, r._2, wos._1, wos._2)
	}

	def getMeanAngle(data: CorporaData): (Double, Double) = calcMeanAngle(data.getVectors)
	def getMeanAngleWithoutSize(data: CorporaData): (Double, Double) = calcMeanAngle(getVectorsWithoutSize(data))

	override def timedMain {
		val data = MetricValuesParser.getData

		apply(data)
	}
	def apply(rawData: CorporaData): Unit = {
		val data = rawData.getAverageValuesPerFile.normalizedByRankings
		val regularData = data.ignoreMetrics(Configuration.ignoredMetrics)
		val ckData = data.useMetrics(Configuration.ckMetrics)
		def getAngles(c: CorporaData) = getMeanAngle(c).toList ++ getMeanAngleWithoutSize(c).toList
		writeLineToLatex("\\itshape Full" :: getAngles(regularData))
		writeLineToLatex("\\itshape C\\&K" :: getAngles(ckData))
	}
}
