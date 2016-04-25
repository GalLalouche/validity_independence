package mains.generators.transformators

import corpus.CorpusData
import corpus.rich.RichCorpusData._
import corpus.rich.RichMetricData._
// A class of transformation that apply a simple f: R => R functionality
trait ComplexTransformator extends LatexNamedTransformator {
  def apply(ds: Seq[Double]): Seq[Double]

  override def apply(data: CorpusData): CorpusData = {
    data.map(_.flatMap(this.apply))
  }
}