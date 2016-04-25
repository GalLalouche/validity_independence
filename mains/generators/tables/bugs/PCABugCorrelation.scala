package mains.generators.tables.bugs

import Jama.Matrix
import corpus.bugs.CorporaDataWithBugs
import mains.generators.transformators.{IndividualPCATransformation, CorpusTransformator, PCATransformation}

trait PCABugCorrelation[T] extends TransformationBugCorrelation[T] {
	protected val pcaMatrix: Matrix
	// this has to be a def, otherwise by the time this is called, the child hasn't had time to init pcaMatrix :\ freaking OOP
	protected override def t: CorpusTransformator = IndividualPCATransformation
}
