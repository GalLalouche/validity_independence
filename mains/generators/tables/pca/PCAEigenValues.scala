package mains.generators.tables.pca

import corpus.CorporaData
import corpus.rich.RichData._
import stats.PCA

/**
 * This class checks the angles and eigenvalues for the PCA matrix vectors
 */
object PCAEigenValues extends PCAMatrixValues {
	override def extractDataForNormalization(data: CorporaData): Seq[Seq[Double]] = {
		def getEigenValues(data: Seq[Seq[Double]]): Seq[Double] = PCA.getVectorsWithEigenValues(data) map (_._2)
		data
			.metricValuesOnly
			.map(getEigenValues)
			.transpose
	}
}
