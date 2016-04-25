package metrics

import java.io.ByteArrayInputStream

import ast.{ASTStringParser, RichASTNode}
import common.rich.RichAll._
import japa.parser.JavaParser
import japa.parser.ast.body.MethodDeclaration
import japa.parser.ast.stmt.BlockStmt
import japa.parser.ast.visitor.{VoidVisitorAdapter, GenericVisitorAdapter, GenericVisitor}
import org.eclipse.jdt.core.dom.ASTNode

object WMC extends Metric {
	override def apply(v1: RichASTNode[_ <: ASTNode]): Double = {
		var bodies = List[String]()
		JavaParser
			.parse(v1.nodeString.toInputStream)
			.accept(new VoidVisitorAdapter[Any]() {
			override def visit(n: MethodDeclaration, arg: Any) {
				if (n.getBody != null)
					bodies = n.getBody.toString :: bodies
			}
		}, null)
		bodies
			.map(ASTStringParser.parse)
			.map(_.tokens.size - 2)
			.sum
	}
}
