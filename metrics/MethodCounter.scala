package metrics

import ast.RichASTNode
import org.eclipse.jdt.core.dom._
import ast.RichASTNode._

object MethodCounter extends Metric {
  override val shortName = "NOM"
  override def apply(v1: RichASTNode[_ <: ASTNode]): Double = {
    v1.collect {
      case e: MethodDeclaration => 1
    }.sum - ConstructorCounter(v1) // do not count C'tors
  }
}
