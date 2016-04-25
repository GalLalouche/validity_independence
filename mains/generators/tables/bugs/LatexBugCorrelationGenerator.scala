package mains.generators.tables.bugs

import common.rich.RichAll._
import common.rich.path.Directory
import corpus.Corpus
import corpus.bugs._
import corpus.rich.RichData._
import mains.generators.tables._
import mains.programs.{Debug, MetricValuesParser}
import metrics.{Metric, NumberOfTokens}
import stats.KendallTau

import scala.collection.mutable.ListBuffer

private[bugs] trait LatexBugCorrelationGenerator[T] extends LatexTableWithDynamicHeaders with Debug {
	override protected val formatTableItem: PartialFunction[Any, Any] = {
		case e: Double if Math.abs(e) < 0.01 => "0"
	}

	protected abstract class AuxBugTables[S] extends LatexTableWithDynamicHeaders {
		override protected val formatTableItem = LatexBugCorrelationGenerator.this.formatTableItem
		override def fileName: String = LatexBugCorrelationGenerator.this.fileName + super.fileName
		override def directory: Directory = LatexBugCorrelationGenerator.this.directory
		// UGLY!
		protected var seqs: Seq[(S, Seq[Any])] = Seq()
		def write() {
			writeTable(None, seqs.map(e => e._1 :: e._2.toList))
		}

		def append(t: S, xs: Seq[Any]) {
			seqs += t -> xs
		}
	}
	protected class MeanPerMetric extends AuxBugTables[Metric] {
		override def write() { writeTable(None, seqs.map(e => e._1.shortName :: e._2.toList)) }
	}

	private class MeanPerCorpus extends AuxBugTables[Corpus] {
	}

	protected val statisticsColumns = new LatexFormulaColumnsBuilder()
		.addMultiColumn("Mean$\\pm\\sigma$", Seq(("Mean", _.mean), ("$\\sigma$", e => e.standardDeviation)))
		.addMultiColumn("Median$\\pm\\text{MAD}$", Seq(("Median", _.median), ("MAD", e => e.medianAbsoluteDeviation)))
		.append("Min", _.min)
		.append("Max", _.max)

	protected def apply(data: MetricDataWithBugs[T]): Double

	private def apply(data: CorpusDataWithBugs[T]): Seq[(Double, Metric)] =
		data.metricValues.map(e => (apply(e), e.m))

	protected lazy val rawData = MetricValuesParser.getData.getAverageValuesPerFile
	protected def createMeanPerMetric: MeanPerMetric = new MeanPerMetric
	def apply(data: CorporaDataWithBugs[T]) {
		def sizeCorrelationFor(m: Metric) = {
			require(rawData.metrics.contains(m), "could not find a match for " + m)
			(100 * rawData
				.average(
					c => KendallTau(c(m).metricValuesOnly,
						c(NumberOfTokens).metricValuesOnly))
				).toInt
		}
		val headers = ("\\textbf{Metric}" :: data.corpora.map(_.c.toString).toList) ++
			statisticsColumns.build.columnNames +
			"{\\raise.17ex\\hbox{$\\scriptstyle\\sim$}}	\\NOT"

		val lines = new ListBuffer[Seq[Any]]
		val meanMetricWriter = createMeanPerMetric
		timed("Calculating bug correlation for " + this.fileName) {
			val meanDifferences = for (meanPerMetric <- data.corpora.par.map(apply).seq.transpose) yield {
				assert(meanPerMetric.hasSameValues(_._2))
				val metric = meanPerMetric.head._2
				val values = meanPerMetric map (_._1)
				val dataWithInfo = statisticsColumns.build apply values
				meanMetricWriter.append(metric, dataWithInfo :+ sizeCorrelationFor(metric))
				lines += metric.shortName :: values.toList ++ (dataWithInfo :+ sizeCorrelationFor(metric))
				values
			}
			val summaryPerProject = new ListBuffer[Seq[Any]]
			for ((str, f) <- statisticsColumns.build.split.zip.removeAt(1).removeAt(2))
				summaryPerProject += str.textbf.align(right) :: (meanDifferences.transpose map f toList)
			meanMetricWriter.write
			val meanCorpusWriter = new MeanPerCorpus
			summaryPerProject
				.transpose
				.drop(1)
				.zip(data.corpora.map(_.c))
				.foreach(e => meanCorpusWriter.append(e._2, e._1))
			meanCorpusWriter.write
			writeTable(Some(headers), lines, summaryPerProject)
		}
	}

}
