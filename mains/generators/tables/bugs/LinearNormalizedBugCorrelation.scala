package mains.generators.tables.bugs

import mains.generators.transformators.{CorpusTransformator, SizeLinearNormalizationTransformation}

trait LinearNormalizedBugCorrelation[T] extends TransformationBugCorrelation[T] {
	override protected val t: CorpusTransformator = SizeLinearNormalizationTransformation
}

