package mains.generators.transformators

import common.rich.collections.RichTraversableDouble.richTraversableDouble

object NormalizedByRankingTransformator extends ComplexTransformator {
  override def apply(ds: Seq[Double]) = ds.normalizedByRankings
  
  override def getLatexHead(name: String) = s"\\text{rank}$name"
}