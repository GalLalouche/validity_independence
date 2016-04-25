package metrics

import ast.RichASTNode
import ast.RichASTNode._
import org.eclipse.jdt.core.dom._

object HorizontalComplexity extends Metric {
  private def isScopeStatement(e: ASTNode) = e match {
    case _: IfStatement ⇒ true
    case _: WhileStatement ⇒ true
    case _: ForStatement ⇒ true
    case _: EnhancedForStatement ⇒ true
    case _: SwitchStatement ⇒ true
    case _: MethodDeclaration ⇒ true
    case _: TryStatement ⇒ true // catch clauses are children of try clauses
    case _ ⇒ false
  }

  override def apply(v1: RichASTNode[_ <: ASTNode]): Double = {
    v1.collect {
      case _: Block => 0
      case e: Statement => e.toRichNode.parents.count(isScopeStatement)
      case _: CatchClause => 1
    }.sum
  }

  override val shortName = "HOR"
}
