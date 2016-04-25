package metrics

import ast.RichASTNode
import org.eclipse.jdt.core.dom.ASTNode

object NumberOfTokens extends Metric {
  override def apply(myNode: RichASTNode[_ <: ASTNode]): Double = myNode.tokens.size
}
