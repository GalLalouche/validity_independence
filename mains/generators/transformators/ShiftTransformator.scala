package mains.generators.transformators

import common.rich.collections.RichTraversableDouble.richTraversableDouble

object ShiftTransformator extends ComplexTransformator {
  override def apply(ds: Seq[Double]) = {
    val mean = ds.mean
    ds map (_ - mean)
  }
  override def getLatexHead(name: String) = s"$name - \\mu"
}