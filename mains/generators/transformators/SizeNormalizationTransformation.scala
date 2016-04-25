package mains.generators.transformators

import common.rich.RichAll.{richSeq, richString, richT, richTraversable}
import corpus.rich.RichCorpusData
import corpus.rich.RichCorpusData.richCorpusData
import corpus.{CorpusData, MetricValuesForFile}
import metrics.Metric

abstract private[transformators] class SizeNormalizationTransformation(val sizeMetric: Metric)
    extends LatexNamedTransformator {
  require(sizeMetric != null)
  protected def modify(data: CorpusData): CorpusData
  def apply(unmodifiedData: CorpusData): CorpusData = {
    val data = modify(unmodifiedData)
    val metrics = data.metrics
    val sizeMetricsIndex = metrics.findIndex(_ == sizeMetric).get
    val normalizedMetrics = metrics map (m => m.withShortName(rename(m)))
    def normalize(values: MetricValuesForFile): MetricValuesForFile = {
      val (sizeMetricValue, sizeMetricMetric) = values.zippedValues(sizeMetricsIndex)
      assert(sizeMetricMetric == sizeMetric)
      require(sizeMetricValue != 0) // in case some ass-hat decided to commit an empty file
      def normalize(e: ((Double, Metric), Metric)): Double =
        e._1._1 / (if (e._2 == sizeMetric) 1.0 else sizeMetricValue)
      new MetricValuesForFile(values, values.zippedValues
          .zip(normalizedMetrics)
          .map(e => (normalize(e), e._2)))
    }
    data
        .transpose
        .tryMap(normalize)
        .mapTo(RichCorpusData.buildFromTransposedData(data.c, _))
  }

  protected def suffix: String
  private def rename(m: Metric): String = m match {
    case _ if m == sizeMetric => "\\" + m.shortName
    case _ => s"$$\\${m.shortName.captureWith("([a-zA-z]+).*".r) }_\\text{$suffix}$$"
  }
  override def getLatexHead(variableName: String): String = s"\\frac{$variableName}{\\text{${sizeMetric.shortName}}}"

  def withSizeMetric(m: Metric): SizeNormalizationTransformation = new SizeNormalizationTransformation(m) {
    override protected def suffix: String = SizeNormalizationTransformation.this.suffix
    protected def modify(data: CorpusData): CorpusData = SizeNormalizationTransformation.this.modify(data)
    override def getLatexHead(variableName: String): String = SizeNormalizationTransformation.this.getLatexHead(variableName)
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[SizeNormalizationTransformation]
  override def equals(other: Any): Boolean = other match {
    case that: SizeNormalizationTransformation =>
      (that canEqual this) &&
          sizeMetric == that.sizeMetric
    case _ => false
  }
  override def hashCode(): Int = {
    val state = Seq(sizeMetric)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
