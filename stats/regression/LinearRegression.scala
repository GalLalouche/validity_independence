package stats.regression

import common.rich.RichAll._
import corpus.CorpusData
import corpus.bugs._
import corpus.rich.RichData._
import org.apache.commons.math3.stat.regression.{OLSMultipleLinearRegression, SimpleRegression}
import stats.RGetter

object LinearRegression {
	def getOls(data: CorpusDataWithBugs[Double]) = {
		val $ = new OLSMultipleLinearRegression
		val transposed: Seq[MetricValuesForFileWithBugs[Double]] = data.transpose
		$.newSampleData(transposed.map(_.bugValue).normalizedByRankings.toArray, transposed.map(_.metricValues.map(_.metricValue)).deepArray)
		$
	}

	def multipleLinearRegression(xss: Seq[Seq[Double]], ys: Seq[Double]): Seq[Double] = {
		val $ = new OLSMultipleLinearRegression
		$.newSampleData(ys.toArray, xss.map(_.toArray).toArray)
		$.estimateRegressionParameters
	}

	def multipleLinearRegression(data: CorpusDataWithBugs[Double]): Seq[Double] = {
		val ols = getOls(data)
		ols.estimateRegressionParameters
	}
	def multipleLinearRegression(data: CorporaDataWithBugs[Double]): Seq[Seq[Double]] =
		data.corpora.map(multipleLinearRegression)

	def calculateUsingRegression(coefs: Seq[Double], data: CorpusData): Seq[Double] = {
		val freeCoef = coefs.head
		val actualCoefs = coefs.tail
		data
			.transpose
			.map(_.metricValues.map(_.metricValue).scalarProduct(actualCoefs) + freeCoef)
	}

	def linearRegression(data: Seq[(Double, Double)]): LinearTransformation = {
		val $ = new SimpleRegression
		$.addData(data.unzip.mapTo(e => Array(e._1, e._2)).map(_.toArray).transpose)
		LinearTransformation(intercept = $.getIntercept, slope = $.getSlope)
	}

	def logisticRegression(data: Seq[(Double, Boolean)]): LogisticTransformation = {
		val f = java.io.File.createTempFile("RData", "logistic data")
		val r = RGetter.get
		try {
			f appendLine "x, y"
			for (entry <- data)
			f.appendLine(s"${entry._1}, ${if (entry._2) 1 else 0}")
			r eval s"""data <- read.csv("${f.path}", header = TRUE)"""
			r eval "fit <- summary(glm(y~x, data = data, family=binomial()))$coefficients"
			val intercept = r.toPrimitive[Double]("fit[1]")
			val slope = r.toPrimitive[Double]("fit[2]")
			LogisticTransformation(intercept = intercept, slope = slope)
		} finally {
			r.quit()
			f.delete()
		}
	}

}
