package metrics

import ast.RichASTNode
import ast.RichASTNode._
import org.eclipse.jdt.core.dom._

import scala.collection.JavaConversions._

private[metrics] trait CommentCharacters {

	def getCommentCharacters(myNode: RichASTNode[_ <: ASTNode]): Iterable[Char] = {
		val cu = myNode.node.asInstanceOf[CompilationUnit]
		val commentAggregator = new CommentVisitor(cu,
			myNode.nodeString.split("\n"),
			getStartPositionOfPackageDeclaration(myNode))
		for (commentNode <- cu.getCommentList.map(_.asInstanceOf[Comment]))
			commentNode.accept(commentAggregator)
		commentAggregator
			.getComments
			.flatMap(_.toCharArray)
	}

	private def getStartPositionOfPackageDeclaration(myNode: RichASTNode[_ <: ASTNode]): Int = {
		var x = 0
		myNode accept new ASTVisitor() {
			override def visit(node: PackageDeclaration) = {
				x = node.getStartPosition
				false
			}
		}
		x
	}
}
