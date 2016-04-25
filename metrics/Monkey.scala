package metrics

import ast.RichASTNode
import org.eclipse.jdt.core.dom.ASTNode

import scala.util.Random

object Monkey extends Metric {
  val r = new Random
  override def apply(v1: RichASTNode[_ <: ASTNode]): Double = r.nextDouble

  override val shortName = "MNK"
}
