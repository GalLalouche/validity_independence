package metrics

import ast.RichASTNode
import org.eclipse.jdt.core.dom.ASTNode

object McCabesDiff extends Metric {
  override def apply(node: RichASTNode[_ <: ASTNode]): Double = McCabesWithShortCircuit(node) - McCabesComplexity(node)
}
