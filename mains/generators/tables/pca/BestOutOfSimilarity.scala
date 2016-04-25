package mains.generators.tables.pca

import common.rich.CacheMap
import common.rich.RichAll._
import corpus.CorpusData
import corpus.rich.RichData._
import mains.generators.tables.FullTableGenerator
import mains.programs.Debug
import mains.programs.pca.{AnglesConsistency, CorpusPair, Vektor}
import stats.{PCA, PairedTTest}

object BestOutOfSimilarity extends FullTableGenerator with Debug {
	private case class CorpusPairSimilarities(mu2: Double, meanSample: Double, medianSample: Double)

	val evCache = CacheMap[CorpusData, Seq[(Vektor, Double)]](_.metricValuesOnly.mapTo(PCA.getVectorsWithEigenValues))
	private val helper = new AnglesConsistency {
		override val pcaCache = evCache.andThen(_.map(_._1))
	}

	private def apply(n: Int, cp: CorpusPair): CorpusPairSimilarities = {
		def sampleBestOfs(bestOf: Int): (Double, Double) = {
			val (c1, c2) = cp.map(helper.antiVectorSample)
			val randomVectorFromC1 = c1.head
			val best = List.fill(1000)(c2.sample(bestOf)).map(_.map(_.cosineSimilarityTo(randomVectorFromC1)).max)
			best.mean -> best.median
		}

		val (mu2, evForMu2) = evCache(cp._1)(1)
		val matchesWithinRatio = evCache(cp._2).sortBy(e => Math.abs(e._2 - evForMu2)).map(_._1).take(n)
		val bestEigenVectorMatch = matchesWithinRatio.map(_.cosineSimilarityTo(mu2)).max
		val bestOfSample = sampleBestOfs(n)
		CorpusPairSimilarities(bestEigenVectorMatch, bestOfSample._1, bestOfSample._2)
	}

	override def timedMain {
		val cps = helper.getCorpora.getUniquePairs.toSeq.par
		for (i <- 1 to 5) {
			val similarities = cps.map(apply(i, _)).seq
			println(s"mean mu_2 angle: ${similarities.map(_.mu2).median }, mean best of angle: ${similarities.map(_.medianSample).median }")
			writeLineToLatex(Seq(i.toString, similarities.map(_.mu2).median, similarities.map(_.medianSample).median))
			PairedTTest(similarities.map(_.mu2), similarities.map(_.medianSample)).log("paired t-test p-value: " + _)
		}
	}
	override protected val caption: String = "Comparison between the median angle of $\\mu_2$ and its match out of a possible $n$ closest eigenvalues"
	override protected val alignments: String = "lll"
	override protected def headers: Seq[String] = Seq("n", "median $\\mu_2$ similarity", "median random angle similarity")
}
