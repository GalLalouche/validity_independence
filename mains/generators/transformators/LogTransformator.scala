package mains.generators.transformators

object LogTransformator extends SimpleTransformator {
  override def apply(d: Double) = {
		require(d > 0)
		Math log d
	}
  override def getLatexHead(name: String) = s"\\log{$name}"
}