package metrics

import ast.RichASTNode
import ast.RichASTNode._
import org.eclipse.jdt.core.dom._

import scala.collection.JavaConversions._

object NumberOfCommentCharacters extends Metric with CommentCharacters {

	override def apply(myNode: RichASTNode[_ <: ASTNode]) = getCommentCharacters(myNode)
		.filterNot(_.isWhitespace).size

	override val shortName = "CCC"
}
