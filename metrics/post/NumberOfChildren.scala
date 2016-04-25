package metrics.post

import ast.RichASTNode
import metrics.Metric
import org.eclipse.jdt.core.dom.ASTNode
import common.rich.RichAll._

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.immutable.Graph

class NumberOfChildren private(classGraph: ClassGraph) extends Metric {
	require(classGraph != null)
	override def apply(v1: RichASTNode[_ <: ASTNode]): Double =
		classGraph
			.find(NumberOfChildren.findMainClassName(v1))
			.expect(s"could not find ${NumberOfChildren.findMainClassName(v1)} in $classGraph")
			.inDegree
}

object NumberOfChildren extends ClassGraphMetricGenerator(new NumberOfChildren(Graph.empty[String, DiEdge])) {
	override def createMetric(cg: ClassGraph): Metric = new NumberOfChildren(cg)
}
