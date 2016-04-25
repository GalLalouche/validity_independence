package mains.programs.pca

import common.rich.CacheMap
import common.rich.RichAll._
import corpus.CorpusData
import corpus.rich.RichData.{richCorporaData, richCorpusData}
import mains.programs.{Configuration, MetricValuesParser}
import stats.PCA

import scala.collection.mutable.ListBuffer
import scala.util.Random

class AnglesConsistency {
	val pcaCache: CorpusData => Seq[Vektor] = CacheMap[CorpusData, Seq[Vektor]](PCA.getVectors)
	// :(((((((((((((((((((
	val list = new ListBuffer[(Double, Double)]
	def similarity(v1: Vektor, v2: Vektor): Double = v1 cosineSimilarityTo v2
	def updateList(corpusPair: (CorpusData, CorpusData), population: Vektor) {
		list += angleForMu2(corpusPair) -> population.mean
	}

	def angleForMu2(cp: CorpusPair): Double = {
		cp
			.map(pcaCache.apply)
			.map(_ (1))
			.mapTo(e => similarity(e._1, e._2))
	}

	def antiVectorSample(c: CorpusData): Seq[Vektor] = {
		val sizeVector = pcaCache(c).head
		c.metricValuesOnly.transpose.map(_.toVector.antiVector(sizeVector)).shuffle
	}

	def getMeanAngleBetweenCorpusPair(corpusPair: (CorpusData, CorpusData)): Double = corpusPair
		.map(antiVectorSample).zip
		.take(5000)
		.map(e => similarity(e._1, e._2))
		.mean

	private def findAnglesInPair(cp: CorpusPair) = {
		def sizeVector(c: CorpusData) = pcaCache(c).head
		val (c1, c2) = cp
		val population = {
			val transposes = cp.map(_.metricValuesOnly.transpose.map(_.toVector))
			(0 to 5000) map { i =>
				val (v1, v2) = transposes.map { e => e.apply(Random.nextInt(e.size)) }
				similarity(v1.antiVector(sizeVector(c1)), v2.antiVector(sizeVector(c2)))
			}
		}
		angleForMu2(cp) -> population.mean
	}

	def corpusPairs: Iterator[(CorpusData, CorpusData)] = {
		getCorpora
			.getUniquePairs
			.toIterator
			.withCounter()
			.par(30)
	}
	def getCorpora: Seq[CorpusData] = {
		MetricValuesParser.getData
			.withoutTests
			.filterLatest
			.normalizedByRankings
			.ignoreMetrics(Configuration.ignoredMetrics)
			.corpora
	}
}
