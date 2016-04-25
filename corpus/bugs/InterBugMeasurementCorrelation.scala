package corpus.bugs

import common.rich.RichAll._
import corpus.rich.RichData._
import stats._

/**
* measures the correlation between two different types of bug measurements
*/
object InterBugMeasurementCorrelation {
	private val c: Correlation = KendallTau
	def apply(c1: CorpusDataWithBugs[Double], c2: CorpusDataWithBugs[Double]) = {
		require(c1.c == c2.c, s"can't compare bugs from different corpora (${c1.c} and ${c2.c})")
		val (c1Bugs, c2Bugs) = c1.transpose.join(c2.transpose)
			.where(_.file == _.file) // join on file name (needed because lengths might be different due to optional values)
			.map(_.map(_.bugValue)) // take bug value from each file
			.unzip.map(_.toSeq)
		c(c1Bugs, c2Bugs)
	}
	def apply(data1: CorporaDataWithBugs[Double], data2: CorporaDataWithBugs[Double]) {
		for ((c1, c2) <- data1.corpora zip data2.corpora)
			println(s"Correlation for corpus ${c1.c} was ${apply(c1, c2)}")
	}
}
