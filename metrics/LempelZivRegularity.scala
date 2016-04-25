package metrics

import alg.LempelZivForStrings
import ast.RichASTNode
import org.eclipse.jdt.core.dom.ASTNode

object LempelZivRegularity extends InverseMetric {
  override def apply(myNode: RichASTNode[_ <: ASTNode]): Double = {
    val tokens = myNode.tokens
    LempelZivForStrings(tokens).size
  }

  override val shortName = "LZW"
}
