package metrics.post

import ast.RichASTNode
import metrics.Metric
import org.eclipse.jdt.core.dom.{ASTNode, TypeDeclaration}

/**
* these are metrics that accept the classes graph
*/
abstract class ClassGraphMetricGenerator(emptyInstance: Metric) extends PostMetric[ClassGraph] with Metric {
	// this class extends metric only for ease of use of equality and hashcode, etc.
	def createMetric(t: ClassGraph): Metric

	override val shortName: String = emptyInstance.shortName

	private[post] def findMainClassName(v1: RichASTNode[_ <: ASTNode]): String = {
		v1
			.find(_.isInstanceOf[TypeDeclaration]).get.asInstanceOf[TypeDeclaration]
			.getName.toString
	}

	override def apply(v1: RichASTNode[_ <: ASTNode]): Double =
		throw new AssertionError("Cannot call apply on an object, please create an instance of the metric using apply")
}
