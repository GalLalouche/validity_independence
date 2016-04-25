package stats

import Jama.Matrix
import common.rich.RichT
import common.rich.collections.RichArray.richArray

import scala.Array.ofDim

trait Correlation extends ((Seq[Double], Seq[Double]) => Double) {

	// convenience method for matrix
	def apply(matrix: Matrix): Array[Array[Double]] = applyAutoCorrelation(matrix.getArray.deepSeq)

	/** Applies the correlation to all possible pairs */
	def applyAutoCorrelation(data: Seq[Seq[Double]]): Array[Array[Double]] = {
		val n = data.size
		val result = ofDim[Double](n, n)
		val (upperTriangle, lowerTriangle) =
			(0 until n)
				.map(i ⇒ (0 until n).map(j ⇒ (i, j))) // <- creates all tuples
				.flatten
				.partition({ case (i, j) ⇒ i < j})
		// only the upper triangle is calculated (not including the main diagonal)
		upperTriangle
			.par
			.foreach({ case (i, j) ⇒ result(i)(j) = this(data(i), data(j))})
		lowerTriangle
			.foreach({
			case (i, j) ⇒
				if (i != j)
					result(i)(j) = result(j)(i) // matrix is symmetric
				else
					result(i)(j) = 1.0 // main diagonal is 1.0 by definition
		})
		result
	}

	override def toString = RichT.richT(this).simpleName
}
