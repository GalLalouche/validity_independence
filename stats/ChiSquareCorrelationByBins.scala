package stats

import common.rich.collections.RichTraversableDouble._
import org.apache.commons.math3.stat.inference.ChiSquareTest

import scala.annotation.tailrec
import scala.annotation.tailrec
import scala.collection.mutable

object ChiSquareCorrelationByBins extends Correlation {
	private val cs = new ChiSquareTest

	override def apply(xs: Seq[Double], ys: Seq[Double]): Double = {
		if (xs.exists(_ < 0) || ys.exists(_ < 0))
			return apply(xs.rerange2Positives, ys.rerange2Positives)
		val maxElement = Math.max(xs.max, ys.max)
		val maxBins = 10000
		val xBinned = xs.fixedSizeBins(maxElement / maxBins)
		val yBinned = ys.fixedSizeBins(maxElement / maxBins)
		val (fixedX, fixedY) = dropDoubleZeroes(xBinned, yBinned)
		if (fixedX.forall(_ == 0) || fixedY.forall(_ == 0))
			return Double.NaN
		if (fixedX.size == 1)
			return Double.NaN
		cs.chiSquareTestDataSetsComparison(fixedX toArray, fixedY toArray)
	}

	private def dropDoubleZeroes(xs: Seq[Long], ys: Seq[Long]): (Seq[Long], Seq[Long]) = {
		val listX = mutable.MutableList[Long]()
		val listY = mutable.MutableList[Long]()

		for ((x, y) <- xs zipAll(ys, 0L, 0L))
			if (x != 0 || y != 0) {
				listX += x
				listY += y
			}
		(listX.drop(1).toVector, listY.drop(1).toVector)
	}
}
