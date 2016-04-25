package mains.generators.tables.dataset

import java.io.File
import java.util.Date

import common.Rope
import common.rich.collections.RichTraversableDouble._
import common.rich.path.RichFile._
import mains.generators.tables.LatexTableGenerator
import mains.programs.Debug
import common.rich.RichT._
import stats.ShapiroWilk

private[tables] object DatasetSummarizer extends LatexTableGenerator with Debug {
	private val SCRIPT_LINE = "% modification by external script"

	override protected val formatTableItem: PartialFunction[Any, Any] = {
		case d: Double => d.toInt
	}

	private def fetchData(f: java.io.File): Seq[Seq[String]] = {
		val $ = f
			.lines
			.takeWhile(_.trim != SCRIPT_LINE)
			.filterNot(_.isEmpty)
			.map(_
			.replaceAll("[\\\\\\s]", "")
			.split("\\s*&\\s*"))
		$ map (_ toVector)
	}

	private def changeDatasetVersionCounter(f: File, columnsToUse: Range) {
		val rawData = fetchData(f).transpose
		f.clear()
		rawData.transpose.foreach(writeLineToLatex(_, f))
		f.appendLine(SCRIPT_LINE)
		f.appendLine("\\hline")
		def column(index: Int) = rawData(index).map(_.toDouble)
		def summarizer(f: Seq[Double] => Any): Seq[Any] = (columnsToUse map column).toList.log(_.foreach(_.log(ShapiroWilk.apply))) map f
		def createLineWithHeader(header: String) = Vector.fill(columnsToUse.start - 1)("") :+ header.wrapInMultiColumn("r", 1)
		writeLineToLatex(createLineWithHeader("Total".wrapInMacro("textbf")) ++ summarizer(_.sum), f)
		writeLineToLatex(createLineWithHeader("Average".wrapInMacro("textbf")) ++ summarizer(_.mean), f)
		writeLineToLatex(createLineWithHeader("STD".wrapInMacro("textbf")) ++ summarizer(_.standardDeviation), f)
		writeLineToLatex(createLineWithHeader("Median".wrapInMacro("textbf")) ++ summarizer(_.median.toInt), f)
		writeLineToLatex(createLineWithHeader("MAD".wrapInMacro("textbf")) ++ summarizer(_.medianAbsoluteDeviation.toInt), f)
		writeLineToLatex(createLineWithHeader("Min".wrapInMacro("textbf")) ++ summarizer(_.min.toInt), f)
		writeLineToLatex(createLineWithHeader("Max".wrapInMacro("textbf")) ++ summarizer(_.max.toInt), f)
	}

	def apply(mains: DatasetMain*) {
		println("Summarizng tables")
		//backup files restores the original file if an exception was thrown because I'm too lazy to discard changes in git
		val originalFiles = mains.map(_.unclearedFile)
		val backupFiles = originalFiles.map(_.backup)
		try {
			if (mains contains DatasetSizeCounter)
				changeDatasetVersionCounter(DatasetSizeCounter.unclearedFile, 1 to 13)
			if (mains contains DatasetCorpusMagnitude)
				changeDatasetVersionCounter(DatasetCorpusMagnitude.unclearedFile, 4 to 5)
			if (mains contains DatasetCommits)
				changeDatasetVersionCounter(DatasetCommits.unclearedFile, 1 to 7)
			if (mains contains DatasetFeatures)
				changeDatasetVersionCounter(DatasetFeatures.unclearedFile, 1 to 12)
		} catch {
			case e: Exception =>
				backupFiles.foreach(_.restore())
				throw e
		}
		println("Done @ " + new Date())
	}

	override def timedMain {
		this(DatasetFeatures)
	}
}
