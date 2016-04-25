package mains.programs

import common.rich.RichAll._
import common.rich.path.Directory
import corpus.rich.RichCorpusData
import corpus.rich.RichData._
import corpus._
import metrics.post.{DepthOfInheritance, NumberOfChildren}
import parsers.StrictCsvFormatter
import java.io.File

/**
 * This class is tasked with all the loading and saving of a given CorpusData to and from a persistent manner
 * (a csv file, in our case)
 */
object MetricValuesParser extends Debug {
	private val metrics =
		(Configuration.softwareMetrics ++ List(NumberOfChildren, DepthOfInheritance))
			.sortBy(_.shortName)
	private val csvFormatter = new StrictCsvFormatter[MetricValuesForFile](
		e => Vector(e.file, e.revision) ++ e.metricValues.map(_.metricValue.toString),
		xs => new MetricValuesForFile(FileCommit.fromStrings(xs(0), xs(1)), xs.drop(2).map(_.toDouble) zip metrics),
		Vector("File", "Revision") ++ metrics.map(_.shortName))
	private val defaultFolder =
		if (new File("c:/dev/git/metrics/Generated/Raw-Metric-Values/").exists)
			Configuration.rawValuesFolder
		else
			throw new IllegalStateException("Cannot access raw values folder; did you forget to mount?")

	private def getFileForCorpus(c: Corpus, outputFolder: Directory) = outputFolder \ s"${c.index}_all.csv"

	// this is not a default value to allow a slightly shorted foreach in MetricValuesGenerator
	def writeMetricValuesToDisk(data: CorporaData, outputFolder: Directory) {
		data.corpora foreach (writeMetricValuesToDisk(_, outputFolder))
	}

	def writeMetricValuesToDisk(data: CorpusData) {
		writeMetricValuesToDisk(data, defaultFolder)
	}

	private def writeMetricValuesToDisk(data: CorpusData, outputFolder: Directory) {
		if (data.metrics != metrics)
			throw new IllegalArgumentException( s"""Metrics do not match Configuration.metrics - this can make it impossible to load later on... are you trying to save data after using ignoreMetrics?
					Was ${data.metrics}
					vs. $metrics""")
		csvFormatter.save(getFileForCorpus(data.c, outputFolder))(data.transpose)

		// verify write by reading it
		try assert(readMetricValuesFromDisk(data.c, outputFolder) != null) // can't really return null
		catch {
			case e: Throwable => throw new AssertionError("could not read from disk after writing to it... something is wrong with the format", e)
		}
	}

	/**
	 * Gets all saved metric data
	 */
	def getData: CorporaData = timed("retrieving data from csv") {
		Configuration.corpora
			.map(readMetricValuesFromDisk(_, defaultFolder))
			.mapTo(new CorporaData(_))
			.ignoreMetrics(Configuration.ignoredMetrics)
			.withoutTests
	}

	/**
	 * Gets saved metric data for a specific corpus
	 */
	def readMetricValuesFromDisk(c: Corpus, fromFolder: Directory = defaultFolder): CorpusData =
		RichCorpusData.buildFromTransposedData(c, csvFormatter.asSequence(getFileForCorpus(c, fromFolder)))

	/**
	 * Finds mismatched indices of corpora, i.e., a mismatch between the saved name of corpus i
	 * and the full name of corpus i as retrieved from the list of corpora
	 */
	def findMismatchedCorpora(fromFolder: Directory = defaultFolder): Seq[(Corpus, (String, String))] =
		Configuration.corpora.zipMap(c => csvFormatter // finds the saved name of corpus in the file
			.asSequence(getFileForCorpus(c, fromFolder))
			.head
			.file
			.captureWith(".*?/?Corpus/([^/]+)/.*".r)
		).filter(e => e._1.fullName != e._2)
			.map(e => e._1 -> (e._1.fullName -> e._2))
}
