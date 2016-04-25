package metrics

import org.eclipse.jdt.core.dom.ASTNode

private[metrics] class SetCountingMetric(nodes: Set[Class[_ <: ASTNode]]) extends CountingMetric {
  require(nodes != null)
  override protected def f: PartialFunction[ASTNode, Boolean] = {
    case e: ASTNode => nodes.contains(e.getClass)
  }
}
