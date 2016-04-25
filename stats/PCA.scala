package stats

import java.io.File

import Jama.Matrix
import common.rich.RichAll._
import corpus.rich.RichData._
import corpus.{CorporaData, CorpusData}
import mains.generators.tables.LatexTableGenerator
import mains.programs.Configuration
import stats.RichMatrix._

import scala.collection.JavaConversions.asScalaBuffer

object PCA extends LatexTableGenerator {

	import mains.generators.tables.LatexTableGenerator._

	def apply(data: Seq[Seq[Double]]): Matrix =
		new Matrix(new PCACalc(data.transpose.deepArray)
			.getDominantComponents(data.size)
			.map(_.eigenVector.reverse)
			.reverse
			.toArray)
			.transpose
	def getEigenValues(data: Seq[Seq[Double]]): Seq[Double] = getVectorsWithEigenValues(data).map(_._2)
	def getVectors(data: CorpusData): Seq[Vector[Double]] = getVectorsWithEigenValues(data.metricValuesOnly).map(_._1)

	def getVectorsWithEigenValues(data: Seq[Seq[Double]]): Seq[(Vector[Double], Double)] = {
		val x = new PCACalc(data.transpose.deepArray)
			.getDominantComponents(data.size)
		val eigenVectors = x.map(_.eigenVector.reverse)
			.reverse
			.transpose
			.map(xs ⇒ if (xs(0) < 0) xs map (-_) else xs)
			.map(_.toVector)
		val eigenValues = x.map(_.eigenValue).toSeq.sorted.reverse
		eigenVectors zip eigenValues
	}
	def transformMetricsValues(data: Seq[Seq[Double]]): Seq[Seq[Double]] = {
		val means = data.map(e ⇒ e.sum / e.size)
		val subtractedMeans = data.zip(means).map({ case (xs, mean) ⇒ xs.map(_ - mean)})
		(apply(subtractedMeans) * new Matrix(subtractedMeans.deepArray)).deepSeq
	}

	override val fileName = "PCA_matrix"

	def apply(data: CorpusData): Matrix = apply(data.metricValuesOnly)

	def apply(data: CorporaData) { apply(data.concatenated) }

	def writePCAMatrixToLatex(data: CorpusData) { writePCAMatrixToLatex(data.metricValuesOnly, data.metrics.map(_.shortName))(file) }
	def writePCAMatrixToLatex(data: Seq[Seq[Double]], metrics: Seq[String] = Configuration.metricShortNames)(outputFile: File = file) {
		assert(data.size == metrics.size, "metric names do not match data wrt size"
			+ s"[data(${data.size}), metrics(${metrics.size})]")
		if (!outputFile.exists())
			outputFile.createNewFile()
		outputFile.clear
		val eigenValues = getEigenValues(data)
		this(data)
			.getArray
			.deepSeq
			.map(_ zip metrics)
			.map(_ sortBy (e ⇒ -(Math abs e._1))) // orders by coefficient's absolute size, desc
			.map(xs ⇒ if (xs(0)._1 < 0) xs map (e ⇒ (-e._1, e._2)) else xs) // assures first term is positive
			.map(_ filter (e ⇒ Math.abs(e._1) > 0.01)) // only takes values larger than 0.01 in abs value
			//      .map(_ take 5) // max number of coefficients fitting in a line
			.map(_ map (e ⇒ f"${e._1}%.2f\\${e._2}"))
			.map(_ mkString "+" replaceAll("\\+\\-", "-")) // replaces +- with -
			.zip(eigenValues)
			.map(e => s"${e._1} (${e._2.toLatexString})") // add eigenvalues data
			.zipWithIndex
			.map(e => f"v_{${e._2 + 1}} = ${e._1}\\\\") // add v_i index
			.foreach(outputFile appendLine _.log())
	}
}
