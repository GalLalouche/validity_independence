package metrics

import ast.RichASTNode
import org.eclipse.jdt.core.dom._

import common.rich.primitives.RichInt._

object EulerTotient extends Metric {
	override def apply(myNode: RichASTNode[_ <: ASTNode]): Double = myNode.nodeString.map(_.toInt).sum.eulersTotient
	override val shortName: String = "ETF"
}
