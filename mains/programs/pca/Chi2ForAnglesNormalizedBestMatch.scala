//package mains.programs
//
//import common.rich.CacheMap
//import common.rich.RichAll._
//import corpus.rich.RichData._
//import corpus.CorpusData
//import stats.{PCA, NormSInv}
//
//import scala.collection.immutable.IndexedSeq
//
//object Chi2ForAnglesNormalizedBestMatch extends Chi2ForAnglesNormalized with Debug {
//	val evCache = CacheMap[CorpusData, Seq[(Vector[Double], Double)]](_.metricValuesOnly.mapTo(PCA.getVectorsWithEigenValues))
//
//	override val pcaCache: Function[CorpusData, Seq[Vector[Double]]] = e => evCache(e).map(_._1)
//	private val ratio = 2
//	override def getAngleForMu2(corpusPair: (CorpusData, CorpusData)): Double = {
//		val (mu2ofc1, ev2ofc1) = evCache(corpusPair._1)(1)
//		evCache(corpusPair._2)
//			.filter(x => (x._2 / ev2ofc1).mapTo(e => e < ratio && e > 1 / ratio))
//			.map(_._1.cosineSimilarityTo(mu2ofc1))
//			.sorted
//			.reverse
////			.tail // comment out to switch between best (commented out) and second-best (uncommented)
//			.head
//	}
//
//	override def timedMain {
//		run()
//	}
//}
