package mains.generators.metricValues

import corpus.bugs.{CorporaDataWithBugs, Feature}

private class FeatureCache private(loader: FeatureLoader = FeatureParser, currentCache: Map[Feature, CorporaDataWithBugs[Double]]) extends FeatureLoader {
	private val map = new scala.collection.mutable.HashMap[Feature, CorporaDataWithBugs[Double]]
	map ++= currentCache
	override def load(f: Feature): CorporaDataWithBugs[Double] = map.getOrElseUpdate(f, loader.load(f))
}
/**
* This object is both a singleton cache and a factory for disposable caches
* (so the GC will collect the cache after it's no longer needed)
*/
object FeatureCache extends FeatureLoader {
	private val globalCache = new FeatureCache(FeatureParser, Map())
	// small optimization to cache: add the global cache to it
	def disposable: FeatureLoader = new FeatureCache(FeatureParser, globalCache.map.toMap)
	override def load(f: Feature): CorporaDataWithBugs[Double] = globalCache load f
}
