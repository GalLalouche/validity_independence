package mains.generators.tables.dataset

import common.rich.RichAll._
import corpus.Corpus
import corpus.bugs.Feature
import corpus.bugs.Feature._
import corpus.rich.RichData._
import mains.generators.metricValues.FeatureCache
import mains.programs.Configuration
import parsers.git.GitAccessor

object DatasetFeatures extends DatasetMain with GitAccessor {

	override protected val formatTableItem: PartialFunction[Any, Any] = {
		case x: Double => x.withPrecision(1)
	}

	private val features = Configuration.goodFeatures
	override def parseInfo(c: Corpus): Seq[Any] = {
		def bugs(f: Feature) = {
			val $ = FeatureCache
				.load(f)
				.find(c).get
				.bugs
			f match {
				case MedianCommitsUntilNextChange => $.map(-_)
				case AverageIsBugInComment => $.map(100 * _)
				case _ => $
			}
		}
		val $ = features
			.map(bugs)
			.flatMap(b => List(b.mean, b.standardDeviation, b.median, b.medianAbsoluteDeviation))
		(0 until 4).foldLeft[Seq[AnyVal]]($) { (agg, i) => // stability should be ints
			agg.updated(i, agg(i).asInstanceOf[Double].toInt)
		}
	}

	override def timedMain {
		Configuration.corpora.foreach(apply)
		DatasetSummarizer.apply(this)
	}
}
