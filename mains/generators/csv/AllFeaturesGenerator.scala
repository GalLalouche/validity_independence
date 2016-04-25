package mains.generators.csv

import mains.generators.metricValues.FeatureParser
import mains.programs.{Configuration, Debug}

object AllFeaturesGenerator extends Debug {
	override def timedMain {
		Configuration.features.foreach(FeatureParser.save)
	}
}
