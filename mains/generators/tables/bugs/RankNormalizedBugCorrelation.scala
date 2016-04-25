package mains.generators.tables.bugs

import mains.generators.transformators.{CorpusTransformator, SizeRankNormalizationTransformation}

trait RankNormalizedBugCorrelation[T] extends TransformationBugCorrelation[T] {
	override protected val t: CorpusTransformator = SizeRankNormalizationTransformation
}

