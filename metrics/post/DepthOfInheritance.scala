package metrics.post

import ast.RichASTNode
import metrics.Metric
import org.eclipse.jdt.core.dom.ASTNode
import scala.annotation.tailrec
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.immutable.Graph

class DepthOfInheritance private(classGraph: ClassGraph) extends Metric {
	require(classGraph != null)
	override def apply(v1: RichASTNode[_ <: ASTNode]): Double = {
		@tailrec
		def aux(currentNode: String, result: Int, visited: Set[String]): Int = classGraph.find(currentNode).get.outgoing.toList match {
		  	case _ if visited(currentNode) => result + 1 // found a cycle... that shouldn't happen :| 	
			case Nil => result
			case x :: Nil => aux(x.target, result + 1, visited + currentNode)
			case x :: xs => throw new AssertionError(s"vertex $currentNode in $classGraph should only have one outgoing edge")
		}
		aux(DepthOfInheritance.findMainClassName(v1), 1, Set())
	}
	override val shortName: String = "DIT"
}

object DepthOfInheritance extends ClassGraphMetricGenerator(new DepthOfInheritance(Graph.empty[String, DiEdge])) {
	override def createMetric(cg: ClassGraph): Metric = new DepthOfInheritance(cg)
	// returns an instance of the metric for configurations, etc.

}



