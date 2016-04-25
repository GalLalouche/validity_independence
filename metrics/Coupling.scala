package metrics

import ast.RichASTNode
import org.eclipse.jdt.core.dom._
import metrics.Chameleonicity.RichType

object Coupling extends Metric with InverseMetric {


  override def apply(v1: RichASTNode[_ <: ASTNode]): Double = {
    v1.collect {
      case e: SimpleType if e.isPolymorphic => e
      case e: QualifiedType if e.isPolymorphic => e
    }
      .filterNot(e => e.toString == "Object" || e.toString == "java.lang.Object")
      .map(_.toString)
      .toSet
      .size
  }

  override val shortName = "CBO"
}
