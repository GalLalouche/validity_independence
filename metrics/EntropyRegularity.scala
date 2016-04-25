package metrics

import ast.RichASTNode
import common.rich.collections.RichTraversable._
import org.eclipse.jdt.core.dom.ASTNode

object EntropyRegularity extends InverseMetric {
  override def apply(myNode: RichASTNode[_ <: ASTNode]): Double = {
    myNode.tokens.entropy
  }
  override val shortName = "ENT"
}
