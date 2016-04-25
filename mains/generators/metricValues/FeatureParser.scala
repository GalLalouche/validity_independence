package mains.generators.metricValues

import corpus.bugs._
import corpus.rich.RichCorpusData
import corpus.rich.RichData._
import corpus.{CorporaData, Corpus, FileCommit}
import mains.programs.{Configuration, Debug, MetricValuesParser}
import metrics.post.{DepthOfInheritance, NumberOfChildren}
import parsers.StrictCsvFormatter

private class FeatureParser(feature: Feature) extends Debug {
	lazy private val metrics =
		(Configuration.softwareMetrics ++ List(NumberOfChildren, DepthOfInheritance))
			.sortBy(_.shortName)
	lazy private val csvFormatter = new StrictCsvFormatter[MetricValuesForFileWithBugs[Double]](
		e => Vector(e.file, e.revision) ++ e.metricValues.map(_.metricValue.toString) :+ e.bugValue.toString,
		xs => new MetricValuesForFileWithBugs(
			FileCommit.fromStrings(xs(0), xs(1)),
			xs.last.toDouble,
			xs.drop(2).dropRight(1).map(_.toDouble) zip metrics
		), Vector("File", "Revision") ++ metrics.map(_.shortName) :+ "featureValue")

	lazy val featureDir = FeatureParser.dir.addSubDir(feature.toString.filterNot(_ == ' '))
	private def getFile(c: Corpus) = featureDir.addFile(c.index.toString + ".csv")
	private def save(data: CorpusDataWithBugs[Double]) {
		csvFormatter.save(getFile(data.c))(data.transpose)
	}

	def save(data: CorporaData) { save(feature joinBugs data) }

	def save(data: CorporaDataWithBugs[Double]) { data.corpora.foreach(save) }
	private def load(c: Corpus): CorpusDataWithBugs[Double] = {
		val file = getFile(c)
		if (file.exists == false)
			throw new IllegalStateException(s"File for corpus $c for feature $feature doesn't exist")
		if (file.lastModified() < c.logFile.lastModified())
			throw new IllegalStateException(s"File for corpus $c (${c.fullName}) for feature $feature is older than the log file; please update the feature file")
		val seq = csvFormatter.asSequence(file)
		RichCorpusData.buildFromTransposedData(c, seq)
	}
	def load: CorporaDataWithBugs[Double] = timed("Loading feature " + feature) {
		new CorporaDataWithBugs[Double](Configuration.corpora.map(this.load(_))).ignoreMetrics(Configuration.ignoredMetrics).withoutTests
	}

	def save() { this.save(feature joinBugs MetricValuesParser.getData) }
}

object FeatureParser extends FeatureLoader with FeatureSaver {
	private val dir = Configuration.rawValuesFolder.parent.addSubDir("Features")
	override def load(f: Feature): CorporaDataWithBugs[Double] = new FeatureParser(f).load
	override protected def save(f: Feature, data: CorpusDataWithBugs[Double]) { new FeatureParser(f).save(data) }
}
