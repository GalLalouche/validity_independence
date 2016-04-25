package stats

import ast.RichASTNode
import metrics.Metric
import org.eclipse.jdt.core.dom.ASTNode

class PCAMetric(val coefficientVector: Seq[(Double, Metric)], index: Int) extends Metric {
	require(coefficientVector nonEmpty)

	override def apply(v1: RichASTNode[_ <: ASTNode]): Double = coefficientVector.map(e => e._1 * e._2(v1)).sum

	override def hashCode = index.hashCode
	override def equals(o: Any) = o match {
		case m: PCAMetric => m.shortName == this.shortName
		case _ => false
	}

	override val shortName = s"$$\\mu_{${index + 1}}$$"
}

object PCAMetric {
	def generateMetrics(count: Int): Seq[PCAMetric] = (0 until count) map (e => new PCAMetric(Seq((0, null)), e))
}
