package metrics

import ast.RichASTNode._
import ast.{ASTNodeVisitor, RichASTNode}
import org.eclipse.jdt.core.dom._

object McCabesComplexity extends SetCountingMetric(
  Set(classOf[WhileStatement], classOf[ForStatement], classOf[DoStatement], classOf[EnhancedForStatement],
    classOf[IfStatement], classOf[SwitchCase], classOf[ConditionalExpression])) {

  // this allows filtering of some of the nodes
  override def apply(v1: RichASTNode[_ <: ASTNode]): Double = super.apply(v1) + 1
  override val shortName: String = "MCCS"
}
