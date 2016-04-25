package metrics

import ast.RichASTNode
import common.rich.collections.RichTraversable.richTraversable
import org.eclipse.jdt.core.dom.{ASTNode, FieldDeclaration, SimpleName, VariableDeclarationFragment}

object LackOfCohesion extends Metric {
  override def apply(v1: RichASTNode[_ <: ASTNode]): Double = {
    val fields = v1.collect {
			case e: VariableDeclarationFragment if e.getParent.isInstanceOf[FieldDeclaration] => e.getName.toString
			case _ => false
		}
      .toSet
    val (emptyIntersections, nonEmptyIntersections) = v1
      .methods
      .map(m =>
      m.collect {
				case e: SimpleName if m.localVariableNames.contains(e.toString) == false => e.toString
			}.toSet.intersect(fields))
      .getUniquePairs
      .foldLeft((0, 0)) {
      case ((emptyIntersectionsAgg, nonEmptyIntersectionsAgg), (xs, ys)) =>
        if (xs.intersect(ys).isEmpty)
          (emptyIntersectionsAgg + 1, nonEmptyIntersectionsAgg)
        else
          (emptyIntersectionsAgg, nonEmptyIntersectionsAgg + 1)
    }
    Math.max(emptyIntersections - nonEmptyIntersections, 0)
  }

  override val shortName = "LCOM"
}