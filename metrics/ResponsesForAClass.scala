package metrics

import ast.RichASTNode
import org.eclipse.jdt.core.dom._

object ResponsesForAClass extends Metric {
  //  override def count(set: Traversable[ASTNode]) = set.collect({

  //  })

  //  .map(_ toString)
  //  .toSet
  //  .size

  override def apply(v1: RichASTNode[_ <: ASTNode]): Double = {
    v1.collect {
      case e: MethodDeclaration => e.getName
      case e: MethodInvocation => e.toString.split("\\(")(0)
      case e: SuperMethodInvocation => "super." + e.getName
    }
      .map(_.toString)
      .toSet
      .size
  }
  override val shortName = "RFC"
}
