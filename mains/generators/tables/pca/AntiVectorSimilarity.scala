package mains.generators.tables.pca

import common.rich.RichAll._
import mains.generators.tables.FullTableGenerator
import mains.programs.Debug
import mains.programs.pca.{AnglesConsistency, CorpusPair, Vektor}
import stats.PairedTTest

object AntiVectorSimilarity extends FullTableGenerator with Debug {
	private case class CorpusPairSimilarities(mu2: Double, meanSample: Double, medianSample: Double)

	private val helper = new AnglesConsistency

	private def apply(evIndex: Int, cp: CorpusPair): CorpusPairSimilarities = {
		def similarity(e: (Vektor, Vektor)): Double = e._1.cosineSimilarityTo(e._2)
		def getMeanMedianSample: (Double, Double) = {
			val (c1, c2) = cp.map(helper.antiVectorSample)
			val angles = c1.shuffle zip c2.shuffle take 1000 map similarity
			angles.mean -> angles.median
		}
		val sample = getMeanMedianSample
		CorpusPairSimilarities(cp.map(helper.pcaCache(_)(evIndex)) |> similarity, sample._1, sample._2)
	}

	override def timedMain {
		val cps = helper.getCorpora.getUniquePairs.toSeq.par
		for (i <- 1 to 10) {
			val similarities = cps.map(apply(i, _)).seq
			writeLineToLatex(Seq(i.toString, similarities.map(_.mu2).median, similarities.map(_.medianSample).median))
		}
	}
	override protected val caption: String = "Comparison between the median similarity of $\\mu_i$ and a two random vectors"
	override protected val alignments: String = "lll"
	override protected def headers: Seq[String] = Seq("EV index", "median $\\mu_i$ similarity", "median random angle similarity")
}
