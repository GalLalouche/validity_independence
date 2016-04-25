package metrics

import ast.RichASTNode
import org.eclipse.jdt.core.dom.{ASTNode, Block, Statement}

object StatementCounter extends CountingMetric {

  override protected def f: PartialFunction[ASTNode, Boolean] = {
    case _: Block ⇒ false
    case _: Statement ⇒ true
  }

  override val shortName = "NOS"
}
