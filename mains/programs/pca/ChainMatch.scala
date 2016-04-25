package mains.programs.pca

import common.rich.RichAll._
import corpus.CorpusData
import mains.programs.Debug
import stats.PCA

import scala.annotation.tailrec

object ChainMatch extends Debug {
	private type Vektors = Map[CorpusData, Seq[Option[Vektor]]]

	private def getBestMatchForVector(eigenVectors: Vektors, v: Vektor, c: CorpusData): (Vektor, Int) =
		eigenVectors(c)
			.zipWithIndex
			.collect { case (Some(v1), x) => v1 -> x }
			.maxBy(_._1.cosineSimilarityTo(v))

	private def chain[T, S](list: Seq[T], s: T => S, f: (S, T) => S): Seq[(S, S)] = {
		list.foldLeft(List((s(list.head), s(list.head)))) {
			case (xs, next) =>
				val last = xs.head._2
				(last, f(last, next)) :: xs
		}.reverse
	}

	override def timedMain {
		val corpora = new AnglesConsistency().getCorpora
		val vectors: Vektors = corpora.zipMap(PCA.getVectors(_).map(_.opt)).asMap
		findMatchingChains(0 to 10, corpora, vectors)
	}

	/** removes the chosen eigen vectors by index, and replace them with a None value */
	def removeChain(xs: Seq[Int], eigenVectors: Vektors): Vektors = {
		require(xs.length == 26, "Not enough elements in the chain")
		xs.zip(eigenVectors).map(e => e._2._1 -> e._2._2.updated(e._1, None)).asMap
	}

	@tailrec
	def findMatchingChains(is: Seq[Int], corpora: Seq[CorpusData], eigenVectors: Vektors) {
		if (is.isEmpty)
			return
		val i = is.head
		print(s"For mu_$i...")
		def aux(f: (Vektor, Int), c: CorpusData): (Vektor, Int) = getBestMatchForVector(eigenVectors, f._1, c)
		val matches = chain[CorpusData, (Vektor, Int)](corpora, eigenVectors(_)(i).get -> i, aux)
		val scores = matches
			.drop(2)
			.+(matches.mapTo(e => e.last._2 -> e.head._1): ((Vektor, Int), (Vektor, Int)))
			.map(e => e._1._1.cosineSimilarityTo(e._2._1))
		println(scores.map(_.withPrecision(2)))
		println(scores.mean)
		println(scores.standardDeviation)
		println(scores.median)
		val matchedIndices = matches.map(_._2._2).drop(1)
		println(matchedIndices)
		findMatchingChains(is.tail, corpora, removeChain(matchedIndices, eigenVectors))
	}
}
