package metrics

import ast.RichASTNode
import org.eclipse.jdt.core.dom._

object McCabesWithShortCircuit extends CountingMetric {

  override protected def f: PartialFunction[ASTNode, Boolean] = {
    case _: EnhancedForStatement ⇒ true
    case _: SwitchCase ⇒ true
    case _: WhileStatement | _: IfStatement | _: ForStatement | _: DoStatement ⇒ true
    case _: ConditionalExpression ⇒ true
    case e: InfixExpression ⇒ Set("&&", "||")(e.getOperator.toString)
  }


  // this allows filtering of some of the nodes
  override def apply(v1: RichASTNode[_ <: ASTNode]): Double = super.apply(v1) + 1

  override val shortName = "MCC"
}
