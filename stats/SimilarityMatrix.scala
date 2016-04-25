package stats

import java.io.File

import Jama.Matrix
import common.rich.RichAll._
import corpus.CorpusData
import corpus.rich.RichCorpusData._
import metrics.Metric

class SimilarityMatrix(data: CorpusData, c: Correlation) extends PartialFunction[(Metric, Metric), Double] {
  private val m = c applyAutoCorrelation data.metricValuesOnly
  private val indices = data.metrics.zipWithIndex.toMap
  override def isDefinedAt(x: (Metric, Metric)): Boolean = x.map(indices.isDefinedAt) ==(true, true)
  override def apply(v: (Metric, Metric)): Double = m.apply(v map indices)

  def toMatrix: Matrix = new Matrix(m)

}
