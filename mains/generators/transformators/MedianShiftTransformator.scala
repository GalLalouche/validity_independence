package mains.generators.transformators

import common.rich.collections.RichTraversableDouble.richTraversableDouble

object MedianShiftTransformator extends ComplexTransformator {
  override def apply(ds: Seq[Double]) = {
    val median = ds.median
    ds map (_ - median)
  }
  override def getLatexHead(name: String) = s"$name - \\mu_{\\sfrac{1}{2}}"
}