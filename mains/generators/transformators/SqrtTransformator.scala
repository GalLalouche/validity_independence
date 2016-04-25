package mains.generators.transformators

object SqrtTransformator extends SimpleTransformator {
  override def apply(d: Double) = Math sqrt d
  override def getLatexHead(name: String) = s"\\sqrt{$name}"
}