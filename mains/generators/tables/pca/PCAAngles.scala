package mains.generators.tables.pca

import java.lang.Math._

import Jama.Matrix
import common.rich.RichAll._
import corpus.CorporaData
import corpus.rich.RichData._
import stats.PCA
import stats.RichMatrix._

/**
 * This class checks the angles the PCA matrix vectors
 */
object PCAAngles extends PCAMatrixValues {
	implicit class richVector(v: Seq[Double]) {
		private def getMagnitude(v: Seq[Double]): Double = sqrt(v map (_.sq) sum)

		def cosineSimilarityTo(other: Seq[Double]): Double = {
			//returns the angle between two vectors
			val dotProduct: Double = v zip other map (e ⇒ e._1 * e._2) sum
			val $ = dotProduct / (getMagnitude(v) * getMagnitude(other))
			$.mapIf(_ > 1).to(e => 1.0).mapIf(_ < -1).to(e => -1.0) // boundaries are between -1 and 1
				.mapTo(Math.abs)
		}
	}

	def anglesBetweenMatrices(m1: Matrix, m2: Matrix) =
		(m1, m2)
			.map(_.rows)
			.zip
			.map(e => e._1 cosineSimilarityTo e._2)

	override def extractDataForNormalization(data: CorporaData) = data
		.metricValuesOnly
		.map(PCA.apply) // pca matrix per corpus
		.getUniquePairs // pca matrix pairs
		.toVector
		.map(e => anglesBetweenMatrices(e._1, e._2))
		.transpose
}
