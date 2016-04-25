package metrics

import ast.RichASTNode
import org.eclipse.jdt.core.dom.ASTNode

trait Metric extends Function[RichASTNode[_ <: ASTNode], Double] {
	// needed for java
	val it: Metric = this

	override def hashCode(): Int = this.getClass.hashCode
	protected def getClassForEquality = this.getClass
	// Objects of the world, unite!
	override def equals(obj: scala.Any): Boolean = obj match {
		case m: Metric => this.getClassForEquality == m.getClassForEquality
		case _ => false
	}
	val shortName: String = getClass.getSimpleName.filter(_.isUpper).mkString
	final override def toString(): String = shortName
	def withShortName(str: String): Metric = new Metric {
		// this method may be used to enable derived metrics symmetric equality with original metric
		override protected def getClassForEquality: Class[_ <: Metric] = Metric.this.getClassForEquality
		override def apply(node: RichASTNode[_ <: ASTNode]): Double = Metric.this.apply(node)
		override val shortName = str
		override def hashCode = Metric.this.hashCode
	}
}
