package metrics

import ast.RichASTNode
import ast.RichASTNode._
import org.eclipse.jdt.core.dom._

object ConstructorCounter extends Metric {
  override def apply(v1: RichASTNode[_ <: ASTNode]): Double = {
    v1.collect {
			case method: MethodDeclaration => method
		}.count(method =>
			method.toRichNode
				.parents.collect {
				case typeNode: TypeDeclaration => typeNode
			}
				.exists(_.getName.toString == method.getName.toString))
  }
  override val shortName = "CON"
}
