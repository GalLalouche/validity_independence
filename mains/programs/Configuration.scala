package mains.programs

import common.rich.path.Directory
import corpus.Corpus
import corpus.bugs.Feature
import mains.generators.metricValues.{FeatureLoader, FeatureParser}
import mains.generators.transformators._
import metrics._
import metrics.post.{NumberOfChildren, DepthOfInheritance}
import parsers.bugs._

object Configuration {
	implicit val softwareMetrics =
		Vector(BranchCounter,
			Chameleonicity, ConstructorCounter, Coupling,
			FieldCounter, GZipRegularity, HorizontalComplexity, LackOfCohesion, LempelZivRegularity,
			LinesOfCode, LoopCounter, McCabesComplexity, McCabesWithShortCircuit, MethodCounter,
			NumberOfCommentCharacters, NumberOfTokens, ResponsesForAClass, StatementCounter, ExplicitMutability, EntropyRegularity,
			NumberOfAlphabeticalCommentCharacters, EulerTotient, Sha1(256), WMC)
			.sortBy(_.shortName) :+ Monkey
	lazy val ckMetrics = Set(WMC, MethodCounter, ResponsesForAClass, DepthOfInheritance, NumberOfChildren, Coupling, LackOfCohesion)
	lazy val ignoredMetrics = Set[Metric](BranchCounter)
	lazy val corpora = Corpus.corpora(Directory("Corpus"))
	lazy val rawValuesFolder = Directory("Generated/Raw-Metric-Values/")
	lazy val generated = Directory("Generated/")
	lazy val metricShortNames = softwareMetrics map (_ shortName)

	lazy val features: Seq[Feature] = Feature.values

	/**
	* Features that average file data and pass normality test
	*/
	lazy val goodFeatures: Seq[Feature] = List(
		Feature.MedianCommitsUntilNextChange,
		Feature.AverageDifferenceSize,
		Feature.AccumulativeIsBugInComment
	)

	lazy val defaultFeature: Feature = Feature.AverageCommitsUntilNextChange

	lazy val namedNormalizations = Vector(
		IdentityTransformator,
		ScaleTransformator,
		ShiftTransformator,
		ShiftTransformator andThen ScaleTransformator,
		LogTransformator andThen ScaleTransformator,
		LogTransformator andThen ShiftTransformator,
		LogTransformator andThen ShiftTransformator andThen ScaleTransformator,
		NormalizedByRankingTransformator)

	lazy val medianNormalizations = Vector(
		MedianShiftTransformator,
		MadScaleTransformator,
		MedianShiftTransformator andThen MadScaleTransformator,
		LogTransformator andThen MedianShiftTransformator,
		LogTransformator andThen MadScaleTransformator,
		LogTransformator andThen MedianShiftTransformator andThen MadScaleTransformator)
	//  .apply(4).asList
}
