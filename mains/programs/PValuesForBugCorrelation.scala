package mains.programs

import corpus.rich.RichData._
import mains.generators.metricValues.FeatureParser
import metrics._
import metrics.post.{DepthOfInheritance, NumberOfChildren}
import stats._

object PValuesForBugCorrelation extends Debug {
	override protected def timedMain {
		val data = MetricValuesParser.getData
		for (f <- Configuration.features) {
			val data = FeatureParser load f
			for (m <- data.metrics.filterNot(Set(DepthOfInheritance, ConstructorCounter, NumberOfChildren, Sha1(12), Monkey).contains)) {
				for (c <- data.corpora)
					if (KendallTau.withPValue(c(m).metricValuesOnly, c.bugs)._2 > 0.05)
						println(s"$m, ${c.c}, $f")
			}

		}
	}
}
