package mains.generators.transformators

import common.rich.collections.RichTraversableDouble._
import corpus.CorpusData
import corpus.rich.RichCorpusData._
import corpus.rich.RichMetricData._

// A class of transformation that apply a simple f: R => R functionality
trait SimpleTransformator extends LatexNamedTransformator {
  def apply(d: Double): Double
  def apply(ds: Seq[Double]): Seq[Double] = ds.rerange2Positives.map(this.apply)

  override def apply(data: CorpusData): CorpusData = {
    data.map(_.flatMap(this.apply))
  }
}