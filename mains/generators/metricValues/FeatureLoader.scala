package mains.generators.metricValues

import corpus.bugs.{CorporaDataWithBugs, Feature}

trait FeatureLoader {
	def load(f: Feature): CorporaDataWithBugs[Double]
}
