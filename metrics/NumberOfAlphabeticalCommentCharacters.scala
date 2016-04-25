package metrics

import ast.RichASTNode
import ast.RichASTNode._
import metrics.NumberOfCommentCharacters._
import org.eclipse.jdt.core.dom._

import scala.collection.JavaConversions._

object NumberOfAlphabeticalCommentCharacters extends Metric with CommentCharacters {

	override def apply(myNode: RichASTNode[_ <: ASTNode]): Double = getCommentCharacters(myNode).count(_.isLetter)

	override val shortName = "ABC3"
}
