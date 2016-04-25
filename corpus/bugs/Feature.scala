package corpus.bugs

import corpus.{CorpusData, CorporaData}
import corpus.rich.RichCorpusData._
import parsers.bugs._

sealed abstract class Feature(f: BugDataGenerator[Double]) extends BugDataGenerator[Double] {
	override def joinBugs(cs: CorporaData): CorporaDataWithBugs[Double] = f joinBugs cs
	override def joinBugs(cs: CorpusData): CorpusDataWithBugs[Double] = f joinBugs cs
	override def toString = f.toString
	def shortName: String = {
		if (toString contains "Commit")
			"Instability"
		else if (toString contains "Comment")
			"Bugginess"
		else if (toString contains "Difference")
			"Change Complexity"
		else
			throw new MatchError(toString)

	}
}

object Feature {
	case object IsBugInComment extends Feature(new BooleanToDoubleConverter(parsers.bugs.IsBugInComment))
	case object AverageIsBugInComment extends Feature(new AverageBugParser(new BooleanToDoubleConverter(parsers.bugs.IsBugInComment)))
	case object AccumulativeIsBugInComment extends Feature(new AccumulativeBooleanCounter(parsers.bugs.IsBugInComment))
	case object AverageBugsAccumulativeMetricValues extends Feature(new AverageBugParser(new BooleanToDoubleConverter(parsers.bugs.IsBugInComment)){
		override protected def aggregateCorpusData(c: CorpusData): CorpusData = c.getSumValuesPerFile
		override def toString: String = "AverageBugsAccumulativeMetricValues"
	})
	case object DifferenceSize extends Feature(parsers.bugs.DifferenceSize)
	case object AverageDifferenceSize extends Feature(new AverageBugParser(parsers.bugs.DifferenceSize))
	case object AccumulativeDifferenceSize extends Feature(new AccumulativeDoubleCounter(parsers.bugs.DifferenceSize))
	case object DifferenceSizeInBuggy extends Feature(parsers.bugs.DifferenceSizeInBuggy)
	case object AverageDifferenceSizeInBuggy extends Feature(new AverageBugParser(parsers.bugs.DifferenceSizeInBuggy))
	case object AccumulativeDifferenceSizeInBuggy extends Feature(new AccumulativeDoubleCounter(parsers.bugs.DifferenceSizeInBuggy))
	case object CommitsUntilNextChange extends Feature(parsers.bugs.CommitsUntilNextChange)
	case object AverageCommitsUntilNextChange extends Feature(new AverageBugParser(parsers.bugs.CommitsUntilNextChange))
	case object MedianCommitsUntilNextChange extends Feature(new MedianBugParser(parsers.bugs.CommitsUntilNextChange))
	case object LifetimeUntilNextChange extends Feature(parsers.bugs.LifetimeUntilNextChange)
	case object AverageLifetimeUntilNextChange extends Feature(new AverageBugParser(parsers.bugs.LifetimeUntilNextChange))
	case object MedianLifetimeUntilNextChange extends Feature(new MedianBugParser(parsers.bugs.LifetimeUntilNextChange))

	val values = List(
		IsBugInComment,
		AverageIsBugInComment,
		AccumulativeIsBugInComment,
		AverageBugsAccumulativeMetricValues,
		DifferenceSize,
		AverageDifferenceSize,
		AccumulativeDifferenceSize,
		DifferenceSizeInBuggy,
		AverageDifferenceSizeInBuggy,
		AccumulativeDifferenceSizeInBuggy,
		CommitsUntilNextChange,
		AverageCommitsUntilNextChange,
		MedianCommitsUntilNextChange,
		LifetimeUntilNextChange,
		AverageLifetimeUntilNextChange,
		MedianLifetimeUntilNextChange
	)

	val averagedFeatures = List(
		AverageIsBugInComment,
		AccumulativeIsBugInComment,
		AverageDifferenceSize,
		AccumulativeDifferenceSize,
		AverageDifferenceSizeInBuggy,
		AccumulativeDifferenceSizeInBuggy,
		AverageCommitsUntilNextChange,
		AverageLifetimeUntilNextChange
	)
}
