package metrics

import ast.RichASTNode
import org.eclipse.jdt.core.dom.ASTNode

import scala.collection.mutable
import scala.collection.mutable.MutableList

private[metrics] trait CountingMetric extends Metric {
  protected def f: PartialFunction[ASTNode, Boolean]
  private val fullF = f.orElse[ASTNode, Boolean] {
    case e: ASTNode => false
  }
  require(f != null)

  // this allows filtering of some of the nodes
  //  def count(set: Traversable[ASTNode]) = set.size
  override def apply(v1: RichASTNode[_ <: ASTNode]): Double = {
    v1.collect(fullF).count(e => e)
  }
}