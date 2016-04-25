package corpus.rich

import corpus.bugs._
import corpus._
import il.ac.technion.cs.ssdl.sablecc.GitLogsRecord

object RichData {
  implicit def richMetricData[T]($: MetricDataWithBugs[T]) = RichMetricData.richMetricDataWithBugs($)
  implicit def richMetricData($: MetricData) = RichMetricData.richMetricData($)
  implicit def richCorpusData[T]($: CorpusDataWithBugs[T]) = RichCorpusData.richCorpusDataWithBugs($)
  implicit def richCorpusData($: CorpusData) = RichCorpusData.richCorpusData($)
  implicit def richCorporaData[T]($: CorporaDataWithBugs[T]) = RichCorporaData.richCorporaDataWithBugs($)
  implicit def richCorporaData($: CorporaData) = RichCorporaData.richCorporaData($)
	implicit def richLogsRecord($: GitLogsRecord) = new RichGitLogsRecord($)
}
