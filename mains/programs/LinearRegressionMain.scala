package mains.programs

import common.rich.RichAll._
import corpus.bugs._
import corpus.rich.RichData._
import mains.generators.metricValues.FeatureParser
import stats._
import stats.regression.LinearRegression

object LinearRegressionMain extends Debug {
	override def timedMain {
		val data = FeatureParser load Configuration.defaultFeature
		val pre = calculateRegressionAccuracy(data)
		println("-----------\nAfter normalizing by ranking...\n-----------")
		val post = calculateRegressionAccuracy(data.normalizedByRankings)
		post zip pre map (e => (e._1._1 - e._2._1, e._1._2 - e._2._2)) zip data.corpora.map(_.c) foreach { e =>
			println(s"Corpus ${e._2} (Pearon, Spearman) improvement is ${e._1.map(_.withPrecision(3))}")
		}
	}
	def calculateRegressionAccuracy(data: CorporaDataWithBugs[Double]) = {
		val x = for (i <- 1 to 100) yield {
			for ((training, test) <- data.sample(2.0 / 3).map(_.corpora).zip) yield {
				val regression = LinearRegression.multipleLinearRegression(training)
				val regressionBugs = LinearRegression.calculateUsingRegression(regression, test)
				val actualBugs = test.transpose.map(_.bugValue)
				(Pearson(regressionBugs, actualBugs), Spearman(regressionBugs, actualBugs))
			}
		}
		x.transpose.map(e => e.unzip.map(e => e.standardDeviation)).log(_ mkString "\n")
	}
}
