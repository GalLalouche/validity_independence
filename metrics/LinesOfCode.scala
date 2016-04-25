package metrics

import ast.RichASTNode
import org.eclipse.jdt.core.dom._

object LinesOfCode extends InverseMetric {
  override def apply(myNode: RichASTNode[_ <: ASTNode]): Double = myNode.nodeString.split("\n").length
}
