package mains.generators.tables.bugs

import common.rich.RichAll._
import common.rich.path.RichFile
import corpus.bugs.{Feature, MetaCorrelation}
import mains.generators.tables.LatexTableGenerator
import mains.generators.transformators._
import mains.programs._
import metrics._

private class MetaCorrelationRSquared(sizeMetric: Metric) extends LatexTableGenerator {
  import MetaCorrelationRSquared._

  private val transformators = createTransformators(sizeMetric)
  def appendSuffix(f: RichFile) =
    f.parent.addFile(s"${f.nameWithoutExtension}${sizeMetric.shortName.toPascalCase}.${f.extension}")
  override def unclearedFile = appendSuffix(super.unclearedFile)
  protected override val formatTableItem: PartialFunction[Any, Any] = {
    case d: Double if d < 0.01 => "0.00"
  }
  private def write(info: MetricInformation) {
    info.rows match {
      case head :: tail =>
        val headers: Seq[Any] = info.m.toString.wrapInMacro("multirow{3}{*}[3mm]") :: head.toList
        writeLineToLatex(headers, file)
        tail.map("" :: _.toList).foreach(writeLineToLatex(_, file))
    }
  }
  private def sortTo[T, S](xs: Seq[T], by: Seq[S], f: T => S): Seq[T] = {
    assert(xs.size == by.size)
    by.map(b => xs.find(f.andThen(_ == b)).get)
  }
  private def sortMetrics(xs: Seq[MetricInformation]) =
    sortTo[MetricInformation, Metric](xs, MetaCorrelation.sizeMetrics(MetaCorrelation.findSizeMetric(transformators)), _.m)
  private def sortFeatures(xs: Seq[Row]) = sortTo[Row, Feature](xs, MetaCorrelation.features, _.f)
  private def sortObservations(xs: Seq[MetaCorrelation]) = sortTo[MetaCorrelation, LatexNamedTransformator](xs, transformators, _.t)
  private def go() {
    val mis: Seq[MetricInformation] = MetaCorrelation.getAll(transformators).groupBy(_.sizeMetric).map {case (m, mcs) =>
      MetricInformation(m, mcs.groupBy(_.feature).map {case (f, observations) =>
        val globalRSquared: MetaCorrelation = observations.reduce(_ ++ _)
        Row.fromSeq(f, (sortObservations(observations) + globalRSquared).map(Math abs _.pearson))
      }.toSeq.mapTo(sortFeatures))
    }.toSeq
    assert(mis.size == 4)
    sortMetrics(mis).foreachWithBetween(write, () => appendMidRule)
  }
}

object MetaCorrelationRSquared extends Debug {
  private case class Row(f: Feature, identityR2: Double, linearR2: Double, rankedR2: Double, globalR2: Double) {
    def toList: Seq[Any] = List(f.shortName, identityR2, linearR2, rankedR2, globalR2)
  }
  private object Row {
    def fromSeq(f: Feature, r2s: Seq[Double]): Row = {
      require(r2s.size == 4)
      this (f, r2s(0), r2s(1), r2s(2), r2s(3))
    }
  }
  private case class MetricInformation(m: Metric, rows: Seq[Row]) {
    require(rows.size == 3)
  }
  def createTransformators(sizeMetric: Metric): Seq[LatexNamedTransformator] = Seq(
    IdentityTransformator,
    new SizeLinearNormalizationTransformation(sizeMetric),
    new SizeRankNormalizationTransformation(sizeMetric))
  override def timedMain {
    Seq(NumberOfTokens, LinesOfCode, LempelZivRegularity)
        .map(new MetaCorrelationRSquared(_)).foreach(_.go())
  }
}
