package mains.generators.transformators

import common.rich.collections.RichTraversableDouble.richTraversableDouble

object MadScaleTransformator extends ComplexTransformator {
  override def apply(ds: Seq[Double]) = {
    val mad = ds.medianAbsoluteDeviation
    assert(mad >= 0)
    if (mad == 0)
      ds
    else
      ds map (_ / mad)
  }

  override def getLatexHead(name: String) = s"\\frac{$name}{\\text{MAD}}"
}