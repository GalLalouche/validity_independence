package mains.generators.tables.bugs

import common.rich.RichAll._
import corpus.Corpus
import corpus.bugs.Feature
import corpus.rich.RichData._
import mains.generators.metricValues.FeatureCache
import mains.generators.tables.MatrixTableGenerator
import mains.programs.{Configuration, Debug}

object FeaturesPerCorpus extends MatrixTableGenerator[Feature, Corpus] with Debug {
	private val cache = FeatureCache

	override def createCell(f: Feature, c: Corpus) = (cache load f find c).get.bugs.mean

	override protected val caption: String = "Captionz"
	override protected val alignments: String = "lrrrrrrrrrrrr"

	//override this to provide your own headers!
	override def customHeaders(columns: Seq[Feature]): Seq[String] = MetricsValidityTtest customHeaders columns
	override protected def timedMain {
		this.apply(Configuration.features, Configuration.corpora)
	}
}
