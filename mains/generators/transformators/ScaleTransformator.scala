package mains.generators.transformators

import common.rich.collections.RichTraversableDouble.richTraversableDouble

object ScaleTransformator extends ComplexTransformator {
  override def apply(ds: Seq[Double]) = {
    val std = ds.standardDeviation
    ds map (_ / std)
  }
  override def getLatexHead(name: String) = s"\\frac{$name}{\\sigma}"
}