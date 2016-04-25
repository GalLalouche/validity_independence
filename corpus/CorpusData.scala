package corpus


class CorpusData(val c: Corpus, val metricValues: Seq[MetricData]) {
	require(metricValues nonEmpty, "Empty corpus data for corpus " + c)
}