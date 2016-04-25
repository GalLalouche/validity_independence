package metrics

import org.eclipse.jdt.core.dom._

private[metrics] class RichExpression private (exp: Expression) {
  private def getChildren(node: ASTNode): Seq[Any] = {
    val list = node.structuralPropertiesForType
    for (i ← 0.until(list.size)) {
      val curr = list.get(i).asInstanceOf[StructuralPropertyDescriptor]
      val child = node.getStructuralProperty(curr)
      child match {
        case value: List[_] => return value
        case _: ASTNode => return Vector(child)
        case _ =>
      }
    }
    Vector()
  }
  require(exp != null)

  //	 we don't use resolve biding as that slows the parsing too much

  private def isBoolean: Boolean = exp match {
    case e: InfixExpression ⇒ Set("&&", "||", "<", "<=", ">", ">=", "==", "!=")(e.getOperator toString)
    case e: PrefixExpression ⇒ e.getOperator.toString == "!"
    case _: BooleanLiteral ⇒ true
    case _ ⇒ exp.getParent match {
      case _: WhileStatement ⇒ true
      case _: IfStatement ⇒ true
      case _: ForStatement ⇒ true
      case _: DoStatement ⇒ true
      case _ ⇒ false
    }
    //		exp mat
    //		try Set("Boolean", "boolean")(expressionWithBinding
    //			.resolveTypeBinding
    //			.getTypeDeclaration
    //			.getName)
    //		catch { case _: NullPointerException => false }

  }
  lazy val isBooleanExpression: Boolean = isBoolean

  val isRoot = exp.getParent.isInstanceOf[Expression] == false
  val isLeaf = getChildren(exp).isEmpty
}

private[metrics] object RichExpression {
  implicit def richExpression(exp: Expression) = new RichExpression(exp)
}