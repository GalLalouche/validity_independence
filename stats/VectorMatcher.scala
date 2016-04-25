package stats

import Jama.Matrix
import mains.generators.tables.LatexTableGenerator
import mains.generators.tables.pca.PCAAngles
import metrics.{Monkey, Sha1}
import stats.RichMatrix._
import scala.annotation.tailrec
import mains.generators.tables.bugs.MetricsValidityTtest
import mains.programs.MetricValuesParser
import common.rich.RichAll._
import corpus.rich.RichData._
import mains.programs.Configuration
import mains.programs.Debug

object VectorMatcher extends LatexTableGenerator with Debug {
	def findBestMatch(pca1: Seq[Seq[Double]], pca2: Seq[Seq[Double]]): (Seq[Int], Seq[Double]) = {
		def permute[T](xs: Seq[T], p: Seq[Int]): Seq[T] = {
			require(xs.size == p.size)
			p map (xs(_))
		}
		def similarities(pca1: Seq[Seq[Double]], pca2: Seq[Seq[Double]]) = {
			val $ = pca1
				.zip(pca2)
				.map(e => e._1.toVector.cosineSimilarityTo(e._2.toVector))
			val negativeNumber = $.findWithIndex(_ < 0)
			assert(negativeNumber.isEmpty, negativeNumber ->
				(pca1(negativeNumber.get._2) -> pca2(negativeNumber.get._2)))
			$
		}
		0.until(pca2.size)
			.permutations
			.map(p => p -> similarities(pca1, permute(pca2, p)))
			.maxBy(_._2.median)
	}
	override def timedMain {
		val data = MetricValuesParser.getData
//			.useMetrics(Configuration.ckMetrics)
			.ignoreMetrics(Configuration.ignoredMetrics)
			.getAverageValuesPerFile
			.normalizedByRankings
		val n = 8
		val pcas = data.corpora
			.map(_.metricValuesOnly)
			.map(PCA.getVectorsWithEigenValues(_).map(_._1))
			.map(_.take(n))
		val firstPCA = pcas.head.log(_.map(_.map(_.withPrecision(3))) mkString "\n")
		pcas(1).log(_.map(_.map(_.withPrecision(3))) mkString "\n")
		val originalMatches = pcas.tail.map(firstPCA.zip(_).map(e => e._1.toVector.cosineSimilarityTo(e._2.toVector))).transpose
		val bestMatches = (for ((otherPCA, i) <- pcas.tail.iterator.par(5).zipWithIndex) yield {
			val bestMatch = findBestMatch(firstPCA, otherPCA)
			println(s"Best match pca 0 for ${i + 1} yield is $bestMatch")
			bestMatch._2
		}).toSeq.transpose
		for (i <- 1 to n)
			writeLineToLatex(List(s"$$\\mu_$i$$", originalMatches(i-1).mean, bestMatches(i-1).mean))
	}
}
