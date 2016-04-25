package mains.programs.pca

import common.rich.CacheMap
import common.rich.RichAll._
import corpus.CorpusData
import corpus.rich.RichData._
import mains.programs.Debug
import stats.{PCA, PairedTTest}

/** Compares the best eigenvector match to the sample of best out of 5 regular cosine similarities */
object BestSample extends Debug {
	val evCache = CacheMap[CorpusData, Seq[(Vector[Double], Double)]](_.metricValuesOnly.mapTo(PCA.getVectorsWithEigenValues))
	private val helper = new AnglesConsistency {
		override val pcaCache = evCache.andThen(_.map(_._1))
	}

	private def compare(n: Int, ratio: Double, cp: CorpusPair): (Double, Double) = {
		def sampleBestOfs(bestOf: Int): Double = {
			val (c1, c2) = cp.map(helper.antiVectorSample)
			c1.take(5000)
				.map(v => c2.sample(bestOf)
					.map(_.cosineSimilarityTo(v))
					.max)
				.mean
		}
		def betweenRatio(d1: Double, d2: Double): Boolean = if (d1 < d2) d2 / d1 < ratio else betweenRatio(d2, d1)

		val c1ChosenVector = evCache(cp._1)(n)
		val c1ChosenEv = c1ChosenVector._2
		//		val matchesWithinRatio = evCache(cp._2).sortBy(e => Math.abs(e._2 - c1ChosenEv)).map(_._1).take(1)
		val matchesWithinRatio = evCache(cp._2).filter(e => betweenRatio(c1ChosenEv, e._2)).map(_._1)
		if (matchesWithinRatio.isEmpty) // ratio wasn't large enough to find any matches, increase ratio and try again
			return compare(n, ratio * 1.01, cp)
		val bestEigenVectorMatch = matchesWithinRatio.map(_.cosineSimilarityTo(c1ChosenVector._1)).max
		val bestOfSample = sampleBestOfs(matchesWithinRatio.length)
		bestEigenVectorMatch -> bestOfSample
	}

	override def timedMain {
		val cps = helper.getCorpora.getUniquePairs.toSeq.par
		val i = 1
		val ratio = 30
		val (angles, pops) = cps.map(compare(i, ratio, _)).seq.unzip

		println(s"mean mu_2 angle: ${angles.mean }, mean best of angle: ${pops.mean }")
		PairedTTest.apply(angles, pops).log("paired t-test p-value: " + _)
	}
}
